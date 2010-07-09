package org.allforgood
package model

import net.liftweb._
import mapper._
import util._
import common._

class PartnerKey extends LongKeyedMapper[PartnerKey] with IdPK {
  def getSingleton = PartnerKey

  object key extends MappedString(this, 256) {
    override def dbIndexed_? = true
  }

  object enabled extends MappedBoolean(this) {
    override def defaultValue = true
  }
}

object PartnerKey extends PartnerKey with LongKeyedMetaMapper[PartnerKey] {
  def valid_?(key: String): Boolean = 
    Props.testMode || find(By(this.key, key), By(enabled, true)).isDefined
}
