package org.snapimpact
package lib

import org.joda.time._
import org.joda.time.format._

import net.liftweb._
import common._
import util.Helpers._

object AfgDate {
  @volatile var calcDateTimeFunc: Option[() => DateTime] = None
  def dateFormatter = DateTimeFormat.forPattern("yyyy-MM-dd")
  def afgnow: DateTime = calcDateTimeFunc.map(_()) getOrElse new DateTime
  def setCalcDate(d: DateTime) { calcDateTimeFunc = Some(() => d) }
  def format(d: DateTime): String = dateFormatter.print(d)
  def parse(s: String): Box[DateTime] = tryo {
    dateFormatter.parseDateTime( s )
  }
  def isFriday(d: DateTime): Boolean = {
    d.getDayOfWeek == DateTimeConstants.FRIDAY
  }
  def incTilFriday(d: DateTime): DateTime = {
    if (isFriday(d)) d
    else incTilFriday(d plusDays 1)
  }
}
