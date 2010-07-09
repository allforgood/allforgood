package org.allforgood
package model

import net.liftweb._
import mapper._
import util._
import common._

class PartnerKey extends LongKeyedMapper[PartnerKey] with IdPK {
  def getSingleton = PartnerKey
}

object PartnerKey extends PartnerKey with LongKeyedMetaMapper[PartnerKey]
