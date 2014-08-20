/**
  * Dependent.scala - Playing with dependent types
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.higherKinds
import scala.language.implicitConversions

trait TypeOver[-I <: AnyRef] {

  type TypeAt[+_ <: I] 

}

// For
//
//   J <: I
//
// we have
//
//   TypeOver[I] <: TypeOver[J]
//
// is this correct?  The idea is that it is by restriction.  If I have a family of
// types varying over the suptypes of I, then I should be able to think of this
// as a family varying over the subtypes of J.
//

// But now I think you need to rethink the idea of what hom here is for.  What is the
// natural notion of hom between these guys?  The only thing I can think of is that
// it should be some kind of natural transformation .....

trait Hom[-I <: AnyRef, S <: TypeOver[I], T <: TypeOver[I]] extends TypeOver[I] {

  // type Test[+J <: I, -K >: J <: I, +L <: J] = S#TypeAt[K] => T#TypeAt[L]

  // Errr ....
  // type TypeAt[+_ <: I] = S#TypeAt[_] => T#TypeAt[_]

}

// trait Pi[-I <: AnyRef, +S <: TypeOver[I]] {

//   def eval[J <: I] : S#TypeAt[J]

// }

// abstract class Sigma[-I <: AnyRef, +S <: TypeOver[I]] {

//   type Local <: I
//   val l : Local

//   // val witness : S#TypeAt[i.type]

// }


