package org.allforgood
package model

import net.liftweb._
import mapper._
import util._
import common._

class TagStoreDB extends LongKeyedMapper[TagStoreDB] with IdPK with GUIDKey {
  def getSingleton = TagStoreDB

  object tag extends MappedString(this, 256) with DBIndexed
}

object TagStoreDB extends TagStoreDB with LongKeyedMetaMapper[TagStoreDB]
