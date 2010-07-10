package org.allforgood
package api

/**
 * Created by IntelliJ IDEA.
 * User: dave
 * Date: Apr 29, 2010
 * Time: 9:26:28 AM
 * To change this template use File | Settings | File Templates.
 */

import model._
import org.joda.time.format.DateTimeFormat
import scala.xml.Elem
//import net.liftweb.json.JsonAST._
//import net.liftweb.json.ShortTypeHints
import net.liftweb.util.Helpers._

sealed class BoolOrString

final case class Bool(value: Boolean) extends BoolOrString
final case class Str(value: String) extends BoolOrString

object VolOppV1 {
  def timeAsInt(t: Option[TimeOlson]): Int = t.map(_.time.roboSplit(":")) match {
    case Some(AsInt(hrs) :: AsInt(mins) :: _) => hrs * 100 + mins
    // TODO: default to noon if we can't figure it out
    case _ => 1200
  }

  def apply(in: VolunteerOpportunity): List[VolOppV1] = MapToV1(in)


  def apply(
          startDate: String,
          minAge: String,
          endDate: String,
          contactPhone: String,
          quality_score: Double,
          detailUrl: String,
          sponsoringOrganizationName: String,
          latlong: String,
          contactName: String,
          addr1: String,
          impressions: Int,
          id: String,
          city: String,
          location_name: String,
          openEnded: BoolOrString,
          pubDate: String,
          title: String,
          base_url: String,
          virtual: String,
          backfill_title: String,
          provider: String,
          postalCode: String,
          groupid: String,
          audienceAge: String,
          audienceAll: String,
          description: String,
          street1: String,
          street2: String,
          interest_count: Int,
          xml_url: String,
          audienceSexRestricted: String,
          startTime: Int,
          contactNoneNeeded: String,
          categories: List[String],
          contactEmail: String,
          skills: String,
          country: String,
          region: String,
          url_short: String,
          addrname1: String,
          backfill_number: Int,
          endTime: Int
          ) = {
    new VolOppV1(
      Some(startDate),
      Some(minAge),
      Some(endDate),
      contactPhone,
      quality_score,
      detailUrl,
      sponsoringOrganizationName,
      latlong,
      contactName,
      addr1,
      impressions,
      id,
      city,
      location_name,
      openEnded,
      pubDate,
      title,
      base_url,
      virtual,
      backfill_title,
      provider,
      postalCode,
      groupid,
      audienceAge,
      audienceAll,
      description,
      street1,
      street2,
      interest_count,
      xml_url,
      audienceSexRestricted,
      startTime,
      contactNoneNeeded,
      categories,
      contactEmail,
      skills,
      country,
      region,
      url_short,
      addrname1,
      backfill_number,
      endTime)
  }
}
class VolOppV1(
        val startDate: Option[String] = None,
        val minAge: Option[String] = None,
        val endDate: Option[String] = None,
        val contactPhone: String,
        val quality_score: Double,
        val detailUrl: String,
        val sponsoringOrganizationName: String,
        val latlong: String,
        val contactName: String,
        val addr1: String,
        val impressions: Int,
        val id: String,
        val city: String,
        val location_name: String,
        val openEnded: BoolOrString,
        val pubDate: String,
        val title: String,
        val base_url: String,
        val virtual: String,
        val backfill_title: String,
        val provider: String,
        val postalCode: String,
        val groupid: String,
        val audienceAge: String,
        val audienceAll: String,
        val description: String,
        val street1: String,
        val street2: String,
        val interest_count: Int,
        val xml_url: String,
        val audienceSexRestricted: String,
        val startTime: Int,
        val contactNoneNeeded: String,
        val categories: List[String],
        val contactEmail: String,
        val skills: String,
        val country: String,
        val region: String,
        val url_short: String,
        val addrname1: String,
        val backfill_number: Int,
        val endTime: Int) {
          def toXML: Elem = 
            <item><title>{title}</title><link>{xml_url}</link><description>{description}</description><pubDate>{pubDate}</pubDate><guid>{xml_url}</guid>
            <fp:id>{id}</fp:id><fp:backfill_number>{backfill_number}</fp:backfill_number><fp:backfill_title>{backfill_title}</fp:backfill_title><fp:groupid>Me21e9cc2a43aa10bdcf0eb1352035a29</fp:groupid><fp:provider>{provider}</fp:provider>
            <fp:startDate>{startDate}</fp:startDate><fp:endDate>{endDate}</fp:endDate><fp:base_url>base_url</fp:base_url><fp:xml_url>{xml_url}</fp:xml_url><fp:url_short>{url_short}</fp:url_short><fp:latlong>{latlong}</fp:latlong>
            <fp:location_name>{location_name}</fp:location_name><fp:interest_count>{interest_count}</fp:interest_count><fp:impressions>0</fp:impressions><fp:quality_score>{quality_score}</fp:quality_score>
            <fp:categories>{categories.mkString(",")}</fp:categories><fp:skills>{skills}</fp:skills><fp:virtual>{virtual}</fp:virtual><fp:addr1>{addr1}</fp:addr1><fp:addrname1>{addrname1}</fp:addrname1>
            <fp:sponsoringOrganizationName>{sponsoringOrganizationName}</fp:sponsoringOrganizationName><fp:openEnded>{openEnded}</fp:openEnded><fp:startTime>{startTime}</fp:startTime><fp:endTime>endTime</fp:endTime>
            <fp:contactNoneNeeded>{contactNoneNeeded}</fp:contactNoneNeeded><fp:contactEmail>{contactEmail}</fp:contactEmail><fp:contactPhone>{contactPhone}</fp:contactPhone><fp:contactName>{contactName}</fp:contactName>
            <fp:detailUrl>{detailUrl}</fp:detailUrl><fp:audienceAll>{audienceAll}</fp:audienceAll><fp:audienceAge>{audienceAge}</fp:audienceAge><fp:minAge>{minAge}</fp:minAge><fp:audienceSexRestricted>{audienceSexRestricted}</fp:audienceSexRestricted>
            <fp:street1>{street1}</fp:street1><fp:street2>{street2}</fp:street2><fp:city>{city}</fp:city><fp:region>{region}</fp:region><fp:postalCode>{postalCode}</fp:postalCode><fp:country>{country}</fp:country></item>
        }

case class RetV1(
        lastBuildDate: String,
        version: Double,
        language: String,
        href: String,
        description: String,
        items: List[VolOppV1])

/*
FixMe Add in Organization lookup once ORganizationStore implemented, sort out addr1, impressions, pubDate,
base_url,backfill_title,
 */
object MapToV1 {
  def apply(in: VolunteerOpportunity): List[VolOppV1] = {
    val fmt = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss");

    for {
      date <- in.dateTimeDurations
      loc <- in.locations
    }  yield new VolOppV1(
      date.startDate.map(fmt.print),
      in.minimumAge.map(_.toString),
      date.endDate.map(fmt.print),
      in.contactInfo.contactPhone.getOrElse(""),
      0.0, // quality_score
      in.detailURL.getOrElse(""),
      in.sponsoringOrganizationIDs.head,
      loc.toLatLngString,
      in.contactInfo.contactName.getOrElse(""),
      "", // addr1
      0, // impressions
      in.volunteerOpportunityID,
      loc.city.getOrElse(""),
      loc.name.getOrElse(""),
      date.openEnded match {
        case Some(yne) => Str(yne.value)
        case None => Str("")
      },
      "", // pub_date
      in.title,
      "", // base_url
      loc.virtual match {
        case Some(yne) => yne.value
        case None => ""
      },
      "", // backfill_title
      "", // provider
      loc.postalCode.getOrElse(""),
      "", // groupid
      "", // audienceAge
      "", // audienceAll
      in.description.getOrElse(""),
      loc.streetAddress1.getOrElse(""),
      loc.streetAddress2.getOrElse(""),
      0, // interest_count
      "", // xml_url
      in.sexRestrictedTo match {
        case Some(yne) => yne.value
        case None => ""
      },
      VolOppV1.timeAsInt(date.startTime),
      "", // contactNoneNeeded
      in.categoryTags,
      in.contactInfo.contactEmail.getOrElse(""),
      in.skills.getOrElse(""),
      loc.country.getOrElse(""),
      loc.region.getOrElse(""),
      "", // url_short
      "", //addrname1
      0, //backfill_number
      VolOppV1.timeAsInt(date.endTime)
      )
  }
}

// "2010-05-08 09:00:00"
