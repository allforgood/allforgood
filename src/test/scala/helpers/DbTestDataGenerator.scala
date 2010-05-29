package helpers

import _root_.java.util.{ArrayList, UUID}
import org.allforgood.model.FootprintFeed
import xml.XML

import net.liftweb._
import util.Helpers._

/**
 * Created by IntelliJ IDEA.
 * User: zkim
 * Date: Feb 20, 2010
 * Time: 9:00:42 PM
 * To change this template use File | Settings | File Templates.
 */

object DbTestDataGenerator {
    def rndStr = UUID.randomUUID.toString
    def rndLong = randomLong(1000)

    def genFootprintFeed = {
      val subject = XML.loadFile("src/test/resources/sampleData0.1.r1254.xml")
      FootprintFeed.fromXML(subject)
    }
}
