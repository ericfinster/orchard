/**
  * Substitution.scala - Methods for substitutions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.editor

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

import orchard.core.cell._
import orchard.core.expression._

abstract class Substitution(wksp : Workspace, defn : Definition, shell : NCell[Option[Expression]]) extends HasEnvironment {

  def activeExpression : Option[NCell[Expression]]

  val dependencyMap : Map[Expression, List[Expression]] = 
    new HashMap[Expression, List[Expression]]

  def addDependency(dep : Expression, filler : Expression) = {
    println("Adding dependency of " ++ filler.toString ++ " on " ++ dep.toString)

    if (dependencyMap.isDefinedAt(dep))
      dependencyMap(dep) = filler :: dependencyMap(dep)
    else
      dependencyMap(dep) = List(filler)
  }

  def setupDependencies = {
    val seq = envOps.toNCellSeq(envRoot)

    seq foreach (ncell => {
      ncell.value match {
        case filler @ Filler(_, _, _) => {
          // Make a framework, grab the faces and set the dependencies
          val framework = new SimpleFramework(ncell map (Some(_)))

          framework(framework.dimension - 1) foreachCell (cell => {
            cell.item match {
              case Some(v @ Variable(_, isThin)) => 
                if (! isThin) addDependency(v, filler)
              case Some(bdry : Filler#Boundary) => 
                if (bdry != filler.MyBoundary && ! bdry.isThin) addDependency(bdry, filler)
              case _ => ()
            }
          })
        }
        case _ => ()
      }
    })
  }

  def replaceInEnvironment(bindings : Map[Expression, Expression]) = {
    envOps.mapExprs(envRoot, (ncell => {
      ncell map (ex => {
        if (bindings.isDefinedAt(ex)) {
          println("Replacing " ++ ex.toString ++ " in ncell " ++ ncell.value.toString)
          bindings(ex)
        } else ex
      })
    }))
  }

  def replaceInIdentifiers(bindings : Map[Expression, Expression]) = {
    envOps.toExprSeq(envRoot) foreach (ex => {
      if (ex.ident.exprRefs exists (ref => bindings.isDefinedAt(ref))) {
        println("Expression " ++ ex.toString ++ " references a bound variable.")

        ex.ident.tokens foreach {
          case et @ ExpressionToken(e) => if (bindings.isDefinedAt(e)) { et.expr = bindings(e) }
          case _ => ()
        }
      }
    })
  }

  val rigidVariables = HashSet.empty[Expression]

  def bindVariable(varExpr : NCell[Expression], expr : NCell[Expression]) = {
    println("Starting substitution: " ++ expr.value.toString ++ " => " ++ varExpr.value.toString)

    varExpr.zip(expr) match {
      case None => println("Selected cell has incompatible shape.")
      case Some(zippedTree) => {
        println("Shape is compatible.")

        val bindings = HashMap.empty[Expression, Expression]
        val thinDeps = HashSet.empty[Expression]

        try {

          zippedTree map {
            case (tgtExpr, srcExpr) => {
              if (tgtExpr == srcExpr) {
                println("Cell match for: " ++ tgtExpr.toString)
              } else {

                if (! tgtExpr.isVariable || rigidVariables.contains(tgtExpr)) {
                    println("Cell " ++ tgtExpr.toString ++ " is rigid and cannot be substituted for.")
                    throw new IllegalStateException
                } else {
                  val varTgt = tgtExpr.asInstanceOf[Variable]

                  println("Attempting subordinate bind: " ++ srcExpr.toString ++ " => " ++ varTgt.toString)

                  if (bindings.isDefinedAt(tgtExpr)) {
                    if (bindings(tgtExpr) != srcExpr) {
                      println("Variable " ++ varTgt.toString ++ " is already bound to " ++ bindings(tgtExpr).toString)
                      throw new IllegalStateException
                    }
                  }

                  if (varTgt.isThin && ! srcExpr.isThin) {
                    println("Cannot bind " ++ srcExpr.toString ++ " to thin variable " ++ varTgt.toString)
                    throw new IllegalStateException
                  }

                  bindings(tgtExpr) = srcExpr

                  if (srcExpr.isVariable) {
                    println("Variable " ++ srcExpr.toString ++ " is now rigid")
                    rigidVariables += srcExpr
                  }

                  if (! varTgt.isThin && srcExpr.isThin) {
                    println("Variable " ++ varTgt.toString ++ " substituted with thin expression " ++ srcExpr.toString)
                    thinDeps += tgtExpr
                  }
                }
              }
            }
          }

          println("Binding phase completed successfully")

          bindings.keySet.foreach (key => {
            println("Deleting " ++ key.toString ++ "@" ++ key.hashCode.toString)
            envOps.delete(envOps.findByExpression(envRoot, key).get)
          })

          // replaceInSheets(bindings)
          replaceInEnvironment(bindings)
          replaceInIdentifiers(bindings)

          // Now recheck necessary cells for universality

          while (thinDeps.size > 0) {
            val newDeps = new HashSet[Expression]

            thinDeps foreach (dep => {
              println("Rechecking universality dependents for " ++ dep.toString)

              if (dependencyMap.isDefinedAt(dep)) {
                dependencyMap(dep) foreach (d => {
                  val ncell : NCell[Expression] = ??? // environment.locateNode(d).get.expr
                  val filler = ncell.value.asInstanceOf[Filler]
                  val framework = new wksp.WorkspaceFramework(ncell map (Some(_)))

                  // Clear the framework to nook state
                  framework.topCell.item = None
                  framework(framework.dimension - 1) foreachCell (cell => {
                    if (cell.item == Some(filler.MyBoundary)) cell.item = None
                  })

                  if (! framework.topCell.isExposedNook)
                    throw new IllegalStateException("Found an ill-defined filler.")

                  if (framework.topCell.isThinBoundary && ! filler.MyBoundary.isThin) {
                    println("Boundary " ++ filler.MyBoundary.toString ++ " has become thin.")

                    filler.bdryIsThin = true
                    newDeps += filler.MyBoundary
                  }
                })
              }

              thinDeps -= dep
            })

            thinDeps ++= newDeps
          }

        } catch {
          case e : IllegalStateException => println("Error during binding phase.")
        }
      }
    }
  }

  def abstractExpression(filler : Filler) = {
    val bindings : Map[Expression, Expression] = new HashMap

    val fillerVariable = Variable(filler.ident, true)
    val boundaryVariable = Variable(filler.bdryIdent, filler.bdryIsThin)

    bindings(filler) = fillerVariable
    bindings(filler.MyBoundary) = boundaryVariable

    //replaceInSheets(bindings)
    replaceInEnvironment(bindings)
    replaceInIdentifiers(bindings)
  }

}

