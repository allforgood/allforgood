package org.allforgood
package model

import net.liftweb._
import mapper._
import util._
import common._

trait GUIDKey {
  self: BaseMapper with Mapper[_] =>
    
  object key extends MappedString[MapperType](this, 256) {
    override def dbIndexed_? = true
  }
}

class DateTimeDB extends LongKeyedMapper[DateTimeDB] with IdPK with GUIDKey {
  def getSingleton = DateTimeDB

  object startTime extends MappedLong(this) with DBIndexed
  object endTime extends MappedLong(this) with DBIndexed
}

object DateTimeDB extends DateTimeDB with LongKeyedMetaMapper[DateTimeDB]
