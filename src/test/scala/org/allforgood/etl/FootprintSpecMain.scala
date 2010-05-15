package org.allforgood
package etl

/**
 * Created by IntelliJ IDEA.
 * User: mark
 * Date: Apr 10, 2010
 * Time: 2:31:49 PM
 */

import org.allforgood.model._

import junit.framework._
import Assert._

import scala.xml.XML
import net.liftweb.json.JsonAST
import net.liftweb.json.JsonDSL._
import net.liftweb.json.Extraction._

object FootprintSpecMain {
  def suite: Test = {
    val suite = new TestSuite(classOf[FootprintSpecMain])
    suite
  }

  var filename:String = ""

  def main(args : Array[String]) {
    filename = args(0)
    _root_.junit.textui.TestRunner.run(suite)
  }
}

class FootprintSpecMain extends TestCase("app") {
  def testFootprintFeed() = {
    val subject = XML.loadFile(FootprintSpecMain.filename)
    val xmlFeed = FootprintFeed.fromXML(subject)
    assertTrue(xmlFeed != null)
  }
}
