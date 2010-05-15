/*
 * package org.snapimpact {
package snippet {

import _root_.scala.xml.{NodeSeq, Text}
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import org.snapimpact.lib._
import Helpers._
/*
import model.{Timeframe, Model}
 */
import java.util.{UUID, Date}
import java.math.BigInteger

class HelloWorld {
  lazy val date: Box[Date] = DependencyFactory.inject[Date] // inject the date

  def howdy(in: NodeSeq): NodeSeq = {
    Helpers.bind("b", in, "time" -> date.map(d => Text(d.toString)))
  }


  /*
  lazy val date: Date = DependencyFactory.time.vend // create the date via factory

  def howdy(in: NodeSeq): NodeSeq = Helpers.bind("b", in, "time" -> date.toString)
  */
}

}
}
*/
