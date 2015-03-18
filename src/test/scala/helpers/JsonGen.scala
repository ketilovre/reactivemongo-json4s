package helpers

import org.scalacheck._
import Gen._
import Arbitrary._
import org.json4s._

import scala.util.Random

/**
 * Generator for Json4s-documents. Adapted from Eric Torreborre's example.
 * http://etorreborre.blogspot.no/2011/02/scalacheck-generator-for-json.html
 */
object JsonGen {

  implicit def arbitraryJsonType: Arbitrary[JValue] = Arbitrary {
    jsonObject(Random.nextInt(6) + 2)
  }

  private def jsonType(depth: Int): Gen[JValue] = oneOf(jsonArray(depth), jsonObject(depth))

  private def jsonArray(depth: Int): Gen[JArray] = for {
    n    <- choose(1, 4)
    vals <- oneOf(listOfN(n, jsonObject(depth)), primitiveArray(n))
  } yield JArray(vals)

  private def jsonObject(depth: Int): Gen[JObject] = for {
    n    <- choose(1, 4)
    ks   <- keys(n)
    vals <- values(n, depth)
  } yield JObject(Map(ks zip vals:_*).map(x => JField(x._1, x._2)).toList)

  private def keys(n: Int) = listOfN(n, alphaStr suchThat(_.nonEmpty))

  private def values(n: Int, depth: Int) = listOfN(n, value(depth))

  private def value(depth: Int) = {
    if (depth < 1) {
      primitive
    } else {
      oneOf(jsonType(depth - 1), primitive)
    }
  }

  private def primitive: Gen[JValue] = {
    oneOf(
      arbitrary[String].map(JString(_)),
      arbitrary[Int].map(JInt(_)),
      arbitrary[Double].map(JDouble(_)),
      arbitrary[Boolean].map(JBool(_)),
      arbitrary[Boolean].map(_ => JNull)
    )
  }

  private def primitiveArray(length: Int): Gen[List[JValue]] = {
    oneOf(
      listOfN(length, arbitrary[String].map(JString(_))),
      listOfN(length, arbitrary[Int].map(JInt(_))),
      listOfN(length, arbitrary[Double].map(JDouble(_))),
      listOfN(length, arbitrary[Boolean].map(JBool(_)))
    )
  }
}