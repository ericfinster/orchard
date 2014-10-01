/**
  * Examples.scala - Examples of the new tree library
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.higherKinds
import scala.language.implicitConversions

import Nats._
import Trees._
import PastingDiagrams._

object TreeExamples {

  val fred0xml : xml.Node = <box><label>4</label><shell><point><box><label>3</label><shell><point><box><label>2</label><shell><point><obj>1</obj></point></shell></box></point></shell></box></point></shell></box>

  val fred1xml : xml.Node = <box><label>13</label><shell><node><value><box><label>12</label><shell><node><value><dot><label>7</label><corolla><point><unit/></point></corolla></dot></value><shell><point><leaf><unit/></leaf></point></shell></node></shell></box></value><shell><point><node><value><box><label>11</label><shell><node><value><box><label>10</label><shell><leaf><unit/></leaf></shell></box></value><shell><point><node><value><dot><label>6</label><corolla><point><unit/></point></corolla></dot></value><shell><point><leaf><unit/></leaf></point></shell></node></point></shell></node></shell></box></value><shell><point><node><value><box><label>9</label><shell><node><value><box><label>8</label><shell><node><value><dot><label>5</label><corolla><point><unit/></point></corolla></dot></value><shell><point><leaf><unit/></leaf></point></shell></node></shell></box></value><shell><point><leaf><unit/></leaf></point></shell></node></shell></box></value><shell><point><leaf><unit/></leaf></point></shell></node></point></shell></node></point></shell></node></shell></box>

  val fred2xml : xml.Node = <box><label>22</label><shell><node><value><box><label>21</label><shell><node><value><dot><label>18</label><corolla><node><value><nil/></value><shell><point><node><value><cons><hd><unit/></hd><tl><nil/></tl></cons></value><shell><point><node><value><cons><hd><unit/></hd><tl><cons><hd><unit/></hd><tl><nil/></tl></cons></tl></cons></value><shell><point><leaf><unit/></leaf></point></shell></node></point></shell></node></point></shell></node></corolla></dot></value><shell><node><value><leaf><nil/></leaf></value><shell><point><node><value><node><value><dot><label>17</label><corolla><node><value><nil/></value><shell><point><node><value><cons><hd><unit/></hd><tl><nil/></tl></cons></value><shell><point><leaf><unit/></leaf></point></shell></node></point></shell></node></corolla></dot></value><shell><node><value><node><value><dot><label>16</label><corolla><leaf><unit/></leaf></corolla></dot></value><shell><leaf><unit/></leaf></shell></node></value><shell><point><node><value><leaf><cons><hd><unit/></hd><tl><nil/></tl></cons></leaf></value><shell><point><leaf><unit/></leaf></point></shell></node></point></shell></node></shell></node></value><shell><point><node><value><leaf><cons><hd><unit/></hd><tl><cons><hd><unit/></hd><tl><nil/></tl></cons></tl></cons></leaf></value><shell><point><leaf><unit/></leaf></point></shell></node></point></shell></node></point></shell></node></shell></node></shell></box></value><shell><node><value><node><value><dot><label>19</label><corolla><node><value><nil/></value><shell><point><leaf><unit/></leaf></point></shell></node></corolla></dot></value><shell><node><value><leaf><nil/></leaf></value><shell><point><leaf><unit/></leaf></point></shell></node></shell></node></value><shell><point><node><value><leaf><cons><hd><unit/></hd><tl><nil/></tl></cons></leaf></value><shell><point><node><value><node><value><box><label>20</label><shell><node><value><dot><label>15</label><corolla><node><value><nil/></value><shell><point><leaf><unit/></leaf></point></shell></node></corolla></dot></value><shell><node><value><leaf><nil/></leaf></value><shell><point><leaf><unit/></leaf></point></shell></node></shell></node></shell></box></value><shell><node><value><node><value><dot><label>14</label><corolla><node><value><nil/></value><shell><point><leaf><unit/></leaf></point></shell></node></corolla></dot></value><shell><node><value><leaf><cons><hd><unit/></hd><tl><cons><hd><unit/></hd><tl><nil/></tl></cons></tl></cons></leaf></value><shell><point><leaf><unit/></leaf></point></shell></node></shell></node></value><shell><point><leaf><unit/></leaf></point></shell></node></shell></node></value><shell><point><leaf><unit/></leaf></point></shell></node></point></shell></node></point></shell></node></shell></node></shell></box>

  val zeroPdFns = implicitly[PdFunctions[_0]]
  val onePdFns = implicitly[PdFunctions[_1]]
  val twoPdFns = implicitly[PdFunctions[_2]]

  val fred0 : Pd[_0, Int] = zeroPdFns.fromXml(fred0xml)(XmlReadable.intReadable)
  val fred1 : Pd[_1, Int] = onePdFns.fromXml(fred1xml)(XmlReadable.intReadable)
  val fred2 : Pd[_2, Int] = twoPdFns.fromXml(fred2xml)(XmlReadable.intReadable)

}
