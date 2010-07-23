package org.allforgood
package model

import lib._

import net.liftweb._
import util.Helpers
import common._
import mapper._

/**
 * This file contains an implementation of disk-based persistance
 *
 * @author dpp
 */

class OppStore extends LongKeyedMapper[OppStore] with IdPK {
  def getSingleton = OppStore

  object key extends MappedString(this, 256) {
    override def defaultValue = Helpers.nextFuncName
    override def dbIndexed_? = true
  }

  object value extends MappedText(this)

  lazy val opp: VolunteerOpportunity = JSONAble.fromJValue(JSONAble.jv(value))

  def opp(o: VolunteerOpportunity): OppStore = this.value(JSONAble.str(o))
}

object OppStore extends OppStore with LongKeyedMetaMapper[OppStore]

private object DiskOpportunityStore extends DiskOpportunityStore

private class DiskOpportunityStore extends OpportunityStore {
  /**
   * Add a record to the backing store and get a GUID
   * the represents the record
   */
  def create(record: VolunteerOpportunity): GUID = 
    GUID(OppStore.create.opp(record).saveMe.key)

  /**
   * read the GUID from the backing store
   */
  def read(guid: GUID): Option[VolunteerOpportunity] = 
    OppStore.find(By(OppStore.key, guid.guid)).map(_.opp)


  /**
   * Read a set of GUIDs from the backing store
   */
  def read(guids: List[(GUID, Double)]): 
  List[(GUID, VolunteerOpportunity, Double)] = {
    val data = Map(OppStore.findAll(ByList(OppStore.key,
                                           guids.map(_._1.guid))).map {
      op => GUID(op.key) -> op.opp
    } :_*)

    guids.flatMap{ case (g, rank) => data.get(g).map(f => (g, f, rank))}
  }

  /**
   * Update the record
   */
  def update(guid: GUID, record: VolunteerOpportunity) = 
    OppStore.find(By(OppStore.key, guid.guid)).
  foreach(_.opp(record).save)


  /**
   * Remove the record from the system
   */
  def delete(guid: GUID): Unit = 
    OppStore.find(By(OppStore.key, guid.guid)).foreach(_.delete_!)
}

class GeoDB extends LongKeyedMapper[GeoDB] with IdPK with GUIDKey {
  import json._
  private implicit def formats = Serialization.formats(NoTypeHints)

  def getSingleton = GeoDB

  object value extends MappedText(this)

  object nonLocated extends MappedBoolean(this) {
    override def dbIndexed_? = true
  }

  lazy val geo: List[GeoLocation] = JsonParser.parse(value).extract

  def geo(o: List[GeoLocation]): GeoDB = 
    this.value(JSONAble.str(Extraction.decompose(o)))
}

object GeoDB extends GeoDB with LongKeyedMetaMapper[GeoDB]


/**
 * The interface to the Geocoded search
 */
private object DiskGeoStore extends DiskGeoStore

private class DiskGeoStore extends GeoStore {

  /**
   * Assocate the GUID and a geo location
   */
  def add(guid: GUID, location: List[GeoLocation]): Unit = 
    {
      remove(guid)
      GeoDB.create.key(guid.guid).geo(location).
      nonLocated(location.find(!_.hasLocation).isDefined).save
    }

  /**
   * Unassocate the GUID and the geo location
   */
  def remove(guid: GUID) = 
    GeoDB.findAll(By(GeoDB.key, guid.guid)).foreach(_.delete_!)

  /**
   * Update the location of a given GUID
   */
  def update(guid: GUID, location: List[GeoLocation]): Unit = 
    add(guid, location)

  /**
   * Find a series of locations that are within a range of the
   * specified location
   * @param location location around which we are looking
   * @param radius radius, in miles
   */
  def find(location: GeoLocation,
           radius: Double,
           filter: GUID => Boolean,
           first: Int,
           max: Int): List[(GUID, Double)] =
	     {
               val geos = GeoDB.findMap(){
                 geo => 
                   val guid = GUID(geo.key)
                 if (filter(guid)) {
                   val dis = 
                     for {
                       loc <- geo.geo
                       distance <- loc.distanceFrom(location) 
                       if distance <= radius
                     } yield distance
                   
                   dis match {
                     case Nil => Empty
                     case xs => Full(xs.map(v => guid -> v))
                   }
                 } else Empty
               }
               
               val view: List[(GUID, Double)] = 
                 geos.flatten
               
               view.sortWith(_._2 < _._2).drop(first).take(max)
	     }
  
  /**
   * Find the GUIDs that do not have a location
   */
  def findNonLocated(first: Int = 0, max: Int = 200): List[GUID] =
    GeoDB.findAll(By(GeoDB.nonLocated, true), 
                  OrderBy(GeoDB.id, Ascending),
                  StartAt(first), MaxRows(max)).
  map(g => GUID(g.key))
}


private object DBDateTimeStore extends DBDateTimeStore

/**
 * The interface to the Date Time filter search
 */
private class DBDateTimeStore extends DateTimeStore {
  /**
   * Assocate the GUID and times
   */
  def add(guid: GUID, times: List[DateTimeDuration]): Unit = {
    DB.use(DefaultConnectionIdentifier) {
      xx =>
        DateTimeDB.bulkDelete_!!(By(DateTimeDB.key, guid.guid))
        for {
          t <- times
          start <- t.startDate
          end <- t.endDate
        } DateTimeDB.create.key(guid.guid).startTime(start).endTime(end).save
    }
  }
    

  /**
   * Unassocate the GUID and the times
   */
  def remove(guid: GUID): Unit = DateTimeDB.bulkDelete_!!(By(DateTimeDB.key, guid.guid))

  /**
   * Update the times of a given GUID
   */
  def update(guid: GUID, times: List[DateTimeDuration]): Unit =
    add(guid, times)

  /**
   * Find a series GUIDs that are in the time/date range.
   * Return the GUID and millis until start
   */
  def find(start: Long, end: Long): List[(GUID, Long)] = {
    val ctm = AfgDate.afgnow.getMillis

    DateTimeDB.findAll(By_>(DateTimeDB.startTime, ctm),
                       By_<(DateTimeDB.endTime, ctm)).
    map(dtb => dtb.key.is -> (ctm - dtb.startTime)).
    sortWith(_._2 < _._2).foldLeft[(Set[String], List[(String, Long)])](
      Set() -> Nil){
        case ((set, list), (key, len)) if !set.contains(key) => (set + key) -> ((key -> len) :: list)
        case (sl, _) => sl
      }._2.map{ case (key, len) => GUID(key) -> len}
  }

  /**
   * Test if the GUID is in the date range
   * TODO: rename to "contains" ?
   */
  def test(start: Long, end: Long)(guid: GUID):Boolean = {
    DateTimeDB.find(By(DateTimeDB.key, guid.guid),
                    By_<(DateTimeDB.startTime, end),
                    By_>(DateTimeDB.endTime, start)).isDefined
  }
}


/**
 * The in-memory implementation of the tag store
 */
private object DBTagStore extends DBTagStore

private class DBTagStore extends TagStore {
  /**
   * Assocate the GUID and a set of tags
   */
  def add(guid: GUID, tagsToAdd: List[Tag]): Unit = 
    DB.use(DefaultConnectionIdentifier) {
      xx => {
        TagStoreDB.bulkDelete_!!(By(TagStoreDB.key, guid.guid))
        for {
          t <- tagsToAdd
        } TagStoreDB.create.key(guid.guid).tag(t.tag).save
      }
    }

  /**
   * Unassocate the GUID and the tags
   */
  def remove(guid: GUID): Unit = synchronized {
    guids.get(guid) match {
      case Some(toRemove) =>
        for {
          tag <- toRemove
        } tags += tag -> (tags(tag) - guid)
      case _ =>
    }
  }

  /**
   * Update the tags associated with a given GUID
   */
  def update(guid: GUID, tags: List[Tag]): Unit = add(guid, tags)

  /**
   *Find a set of GUIDs assocaiated with a set of tags
   */
  def find(tagsToFind: List[Tag],
           first: Int = 0, max: Int = 200): List[GUID] =
	     {
	       val toFind = TreeSet(tagsToFind :_*)
	       val tg = synchronized {tags}

	       val ret: SortedSet[GUID] = for {
		 t <- toFind
		 lst <- tg.get(t).toList
		 item <- lst
	       } yield item

	       ret.drop(first).take(max).toList
	     }
}

private object DiskLuceneStore extends DiskLuceneStore

/**
 * THe interface for storing stuff in a search engine
 */
private class DiskLuceneStore extends SearchStore {
  private val guid = GUID.create

  override def toString = "DiskLuceneStore "+guid

  import java.io.IOException;
  import java.io.StringReader;

  import org.apache.lucene.search._
  import org.apache.lucene.document._
  import org.apache.lucene.search._
  import org.apache.lucene.store._
  import org.apache.lucene.index._
  import org.apache.lucene.analysis.standard._
  import org.apache.lucene.util.Version
  import org.apache.lucene.queryParser.QueryParser

  private val idx = {
    // FIXME -- disk directory
    val rd = new RAMDirectory()
    val writer = new IndexWriter(rd, new StandardAnalyzer(Version.LUCENE_30),
                                 true, IndexWriter.MaxFieldLength.UNLIMITED)
    writer.optimize
    writer.commit
    writer.close

    rd
  }

  private lazy val writer = new IndexWriter(idx,
                                            new StandardAnalyzer(Version.LUCENE_30),
                                            false, IndexWriter.MaxFieldLength.UNLIMITED)
  
  private var writeCnt = 0L



  private def write[T](f: IndexWriter => T): T = synchronized {
    val ret = f(writer)
    writeCnt += 1
    if (writeCnt % 1000L == 0) {
      writer.optimize
    }

    writer.commit

    ret
  }

  /**
   * Assocate the GUID and an item.
   * @param guid the GUID to associate
   * @param item the item to associate with the GUID
   * @splitter a function that returns the Strings and types of String (e.g., description,
   */
  def add[T](guid: GUID, item: T)(implicit splitter: T => Seq[(String, Option[String])]): Unit = {
    val doc = new Document()

    doc.add(new Field("guid", guid.guid, Field.Store.YES, Field.Index.ANALYZED))

    splitter(item).foreach {
      case (value, name) => doc.add(new Field(name getOrElse "body", value,
					      Field.Store.YES, Field.Index.ANALYZED))
    }

    write(_.addDocument(doc))
  }

  /**
   * Unassocate the GUID and words
   */
  def remove(guid: GUID): Unit = {
    val reader = IndexReader.open(idx)
    reader.deleteDocuments(new Term("guid", guid.guid))
    reader.close()
  }

  /**
   * Assocate the GUID and a set of words
   */
  def update[T](guid: GUID, item: T)(implicit splitter: T => Seq[(String, Option[String])]): Unit = {
    remove(guid)
    add(guid, item)(splitter)
  }


  /**
   * Find a set of GUIDs assocaiated with a search string, optionally
   * specifing the subset of GUIDs to search and the number of results to
   * return
   */
  def find(search: String,
           filter: GUID => Boolean,
           first: Int = 0, max: Int = 200,
           inSet: Option[Seq[GUID]] = None): List[(GUID, Double)]
  = {
    try {
      val qp = new QueryParser(Version.LUCENE_CURRENT, "body",
			       new StandardAnalyzer(Version.LUCENE_30))

      qp.setAllowLeadingWildcard(true)
      val q = qp.parse(search)
      
      val collector = TopScoreDocCollector.create(max + first, true)
      
      val searcher = new IndexSearcher(idx, true)
      
      searcher.search(q, collector)
      
      val hits = collector.topDocs().scoreDocs// .drop(first)

      val ret = for {
	h <- hits.toList
	doc <- (Box !! searcher.doc(h.doc, new MapFieldSelector("guid"))).toList
	guid <- (Box !! doc.getField("guid")).toList
	value <- (Box !! guid.stringValue).map(GUID.apply).toList if filter(value)
      } yield value -> h.score.toDouble

      ret.drop(first).take(max)
    } catch {
      case pe: org.apache.lucene.queryParser.ParseException => Nil
    }
  }
}
