package org.snapimpact.model

import org.specs.Specification
import org.slf4j.LoggerFactory

import org.specs._
import org.specs.runner._
import org.specs.Sugar._

import net.liftweb.util._
import org.snapimpact.etl._
import org.snapimpact.etl.model.dto._

class SearchStoreTest extends Runner(new SearchStoreSpec) with 
JUnit with Console

class SearchStoreSpec extends Specification {
  private lazy val searchStore: SearchStore = 
    new MemoryLuceneStore

  private lazy val opStore = new MemoryOpportunityStore

  lazy val guid1 = GUID.create
  lazy val guid2 = GUID.create
  lazy val guid3 = GUID.create
  lazy val guid4 = GUID.create
  lazy val guid5 = GUID.create

  implicit def strToStore(in: String): Seq[(String, Option[String])] = 
    List(in -> None)

  "Search Store" should {
    "Support storing and searching" in {
      searchStore.add(guid1, "I like to eat fruit")
      
      searchStore.find("eat", x => true).map(_._1) must_== List(guid1)
    }

    "Searching works with a second record" in {
      searchStore.add(guid1, "I like to eat fruit")
      searchStore.add(guid2, "Moose are my favorite fruit")
      
      searchStore.find("eat", x => true).take(1).map(_._1) must_== List(guid1)
      searchStore.find("fruit", x => true).length must_== 2
    }


    "You can search for a VolOpp" in {
      val item = VolunteerOpportunity.fromXML(Map(), FootprintRawData.subject)
      
      val guid = opStore.create(item)

      searchStore.add(guid, item)

      searchStore.find("quest", x => true).length must_== 1
    }

    "Search data in a reasonable corpus" in {
      ReasonableCorpus.corpus.search(query = Some("dodgeball")).length must be > 0
    }
  }
}

object ReasonableCorpus {
  lazy val corpus = {
    import scala.xml.XML
    import java.io.{File => JFile}
    import Helpers._
    import net.liftweb.common._

    import org.snapimpact.lib.AfgDate
    import org.joda.time._

    AfgDate.calcDateTimeFunc = Some(() => new DateTime("2008-12-28"))


    class MyStore extends Store {
      protected lazy val store = new MemoryOpportunityStore
      protected lazy val geo = new MemoryGeoStore
      protected lazy val tag = new MemoryTagStore 
      protected lazy val search = new MemoryLuceneStore
      protected lazy val dateTime = new MemoryDateTimeStore
    }

    var st = new MyStore

    val toTry = new JFile("src/test/resources/HockeyData.xml") ::
    new JFile("src/test/resources/sampleData0.1.r1254.xml")  :: (for {
      dir <- tryo{new JFile("./docs/test_data")}.toList
      files <- (Box !! dir.listFiles).toList
      file <- files if file.getName.endsWith(".xml")
    } yield file)

    toTry.foreach {
      f =>
        tryo {
          println("Loading "+f+" into store")
          val h = XML.loadFile(f)
          val hf = FootprintFeed.fromXML(h)
          hf.opportunities.opps.foreach{ a => st.add(a)}
          println("Finished loading "+f)
        }
    }

    st
  }
}


