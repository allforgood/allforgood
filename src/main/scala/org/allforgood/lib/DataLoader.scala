package org.allforgood
package lib

import net.liftweb._
import common._
import util._

import model._
import java.io.{File, FileInputStream}

object DataLoader extends Loggable {
  def loadStatic() {
    // don't load data in test mode
    if (!Props.testMode) {
      logger.info("Starting static load")

      val store = PersistenceFactory.store.vend

      def loadIt(file: File) {
        // only load this one partner
        if (file.getPath.indexOf("idealist") >= 0) {
          logger.info("Started to load "+file)
          try {
            val xml = PCDataXmlParser(new FileInputStream(file)).open_!(0)
            val info = FootprintFeed.fromXML(xml)
          info.opportunities.opps.foreach(store.add _)
            logger.info("Loaded "+file)
          } catch {
            case e: Exception => logger.error("Failed to load "+file, e)
          }
        }
      }

      val root = new File(System.getProperty("user.home")+"/afg_data")
      if (root.exists && root.isDirectory) {
        root.listFiles.
        filter(f => f.isFile && f.getPath.endsWith(".xml")).
        foreach(loadIt)
      }

      logger.info("Finished static load")
    }
  }
}
