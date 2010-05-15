package org.allforgood
package lib

import org.specs.Specification
import org.allforgood.lib.Serializers._
import org.allforgood.model._

import org.specs._
import org.specs.runner._

class SerializationTest extends Runner(new SerializersSpec) with JUnit with Console

class SerializersSpec extends Specification {
  "anyToRss" should {

     "serialize a Location" in { 
       anyToRss(
         Location(Some(No), Some("name"), Some("streetAddress1"), Some("streetAddress2"),
           Some("streetAddress3"), Some("city"), Some("region"), Some("postalCode"),
           Some("country"), Some((1.23).asInstanceOf[Float]),
           Some((3.12).asInstanceOf[Float]), Some("directions"))
       ).toString mustMatch("<fp:value>No</fp:value>") 
     }
  }
}
