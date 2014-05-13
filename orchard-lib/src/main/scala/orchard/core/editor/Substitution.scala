/**
  * Substitution.scala - Methods for substitutions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.editor

import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.Buffer

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

  def canBind(rigidVars : Set[Expression], boundExpr : NCell[Expression], bindingExpr : NCell[Expression]) : Boolean = {

    boundExpr.zip(bindingExpr) match {
      case None => false
      case Some(zippedTree) => {

        val bindings = HashMap.empty[Expression, Expression]

        try {
          zippedTree map {
            case (tgtExpr, srcExpr) => {
              if (tgtExpr != srcExpr) {
                if (! tgtExpr.isVariable || rigidVars.contains(tgtExpr)) {
                  throw new IllegalStateException
                } else {
                  val varTgt = tgtExpr.asInstanceOf[Variable]

                  if (bindings.isDefinedAt(tgtExpr)) {
                    if (bindings(tgtExpr) != srcExpr) {
                      throw new IllegalStateException
                    }
                  }

                  if (varTgt.isThin && ! srcExpr.isThin) {
                    throw new IllegalStateException
                  }

                  bindings(tgtExpr) = srcExpr
                }
              }
            }
          }

          // I think we should be okay ....
          true

        } catch {
          case e : IllegalStateException => false
        }
      }
    }
  }

  // BUG!!! - When you make substitutions, you may need to update the dependency lists.  Fix this.

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
                  val ncell : NCell[Expression] = envOps.getNCell(envOps.findByExpression(envRoot, d).get)
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

  def abstractExpression(filler : Filler) : Variable = {
    val bindings : Map[Expression, Expression] = new HashMap

    val fillerVariable = Variable(filler.ident, true)
    val boundaryVariable = Variable(filler.bdryIdent, filler.bdryIsThin)

    bindings(filler) = fillerVariable
    bindings(filler.MyBoundary) = boundaryVariable

    //replaceInSheets(bindings)
    replaceInEnvironment(bindings)
    replaceInIdentifiers(bindings)

    if (! filler.bdryIsThin) {
      if (dependencyMap.isDefinedAt(filler.MyBoundary)) {
        // Should really delete the old key as well ...
        dependencyMap(boundaryVariable) = dependencyMap(filler.MyBoundary)
      }
    }

    fillerVariable
  }

  def unifyFillers(unifyVariables : Boolean) = {

    val conflicts = Buffer.empty[Expression]

    envOps.toExprSeq(envRoot) foreach {
      case f : Filler => {
        if (wksp.envOps.containsId(wksp.envRoot, f.id)) {
          println("Adding " ++ f.id ++ " to list of unifiable expressions")
          conflicts += f
        } else {
          println("Skipping non-conflicting expression " ++ f.id)
        }
      }
      case _ => ()
    }

    // Now what?  You keep a size counter and loop through the buffer.  When you succeed in
    // unifying an element, you remove it from the list.  Continue until the list is empty
    // or there is no progress.

    var done : Boolean = false

    // while (! done) {

      conflicts foreach (f => {
        // So I think we need to write a test first to make sure that will happen ....

        // Grab the expressions
        val substExpr = envOps.getNCell(envOps.findByExpression(envRoot, f).get)
        val wkspExpr = wksp.envOps.getNCell(wksp.envOps.findById(wksp.envRoot, f.id).get)

        val filler = f.asInstanceOf[Filler]
        val fillerFramework = new SimpleFramework(substExpr map (Some(_)))

        val fillerVariable = Variable(filler.ident, true)
        val boundaryVariable = Variable(filler.bdryIdent, filler.bdryIsThin)

        fillerFramework.topCell.item = Some(fillerVariable)
        fillerFramework(fillerFramework.dimension - 1) foreachCell (cell => {
          cell.item foreach (i => {
            if (i == filler.MyBoundary) {
              cell.item = Some(boundaryVariable)
            }
          })
        })

        val rigidVars = HashSet.empty ++ rigidVariables
        val finalExpr = fillerFramework.toCell map {
          case Some(e) => {
            if (e != fillerVariable && e != boundaryVariable) {
              rigidVars += e
            }

            e
          }
          case _ => ???
        }

        if (canBind(if (unifyVariables) rigidVariables else rigidVars, finalExpr, wkspExpr)) {
          println("Conflict with " ++ f.id ++ " is unifiable. Trying ...")

          val newVar = envOps.getNCell(envOps.findByExpression(envRoot, abstractExpression(filler)).get)

          bindVariable(newVar, wkspExpr)
        } else {
          println("Binding would fail for " ++ f.id)
        }
      })


    // }
  }
}

