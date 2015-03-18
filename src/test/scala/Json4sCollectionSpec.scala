import com.ketilovre.reactivemongo.json4s.Json4sCollection
import org.json4s._
import org.json4s.JsonDSL._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import reactivemongo.api.MongoDriver
import reactivemongo.bson.BSONObjectID

import scala.concurrent.Await
import scala.concurrent.duration._

class Json4sCollectionSpec extends Specification with ScalaCheck {

  import com.ketilovre.reactivemongo.json4s.ImplicitJson4sHandlers._
  import helpers.JsonGen._

  implicit val formats = org.json4s.DefaultFormats

  val connection = new MongoDriver().connection(List("localhost"))

  Await.result(connection("testDb").drop(), Duration(5, SECONDS))

  def getCollection(name: String) = connection("testDb").collection[Json4sCollection](name)

  "Json4sCollection" should {

    "insert" ! prop { (json: JValue) =>

      val collection = getCollection("properties")

      val id = BSONObjectID.generate.stringify
      val withId = json merge JObject(JField("_id", JString(id)))

      collection.insert(withId).map(_.ok must beTrue).await

      collection.find(JObject(JField("_id", JString(id)))).cursor[JObject].headOption.map(_ must beSome(withId)).await

    }

    "update" in {

      val collection = getCollection("updates")

      val doc = ("name" -> "adam") ~ ("age" -> 3)
      val query1: JValue = "name" -> "adam"
      val query2: JValue = "name" -> "Adam"

      val update1: JValue = "$inc" -> ("age" -> 1)
      val update2: JValue = "$set" -> ("name" -> "Adam")

      collection.insert(doc).map(_.ok must beTrue).await
      collection.find(query1).cursor[JObject].headOption.map(_ must beSome(doc))

      collection.update(query1, update1).map(_.ok must beTrue).await
      collection.find(query1).cursor[JObject].headOption.map(doc =>
        doc.get \ "age" mustEqual JInt(4)
      ).await

      collection.update(query1, update2).map(_.ok must beTrue).await
      collection.find(query2).cursor[JObject].headOption.map(doc =>
        doc.get \ "name" mustEqual JString("Adam")
      ).await

    }

    "sort" in {

      val collection = getCollection("sorting")

      val json1 = ("name" -> "adam") ~ ("age" -> 3)
      val json2 = ("name" -> "xander") ~ ("age" -> 47)

      collection.insert(json1).map(_.ok must beTrue).await
      collection.insert(json2).map(_.ok must beTrue).await

      collection.find(JObject()).sort(JObject(JField("name", 1))).cursor[JObject].headOption.map(doc => {
        doc.get \ "name" mustEqual JString("adam")
      }).await
      collection.find(JObject()).sort(JObject(JField("age", -1))).cursor[JObject].headOption.map(doc => {
        doc.get \ "name" mustEqual JString("xander")
      }).await

      collection.find(JObject()).sort(JObject(JField("age", 1))).cursor[JObject].headOption.map(doc => {
        doc.get \ "age" mustEqual JInt(3)
      }).await
      collection.find(JObject()).sort(JObject(JField("age", -1))).cursor[JObject].headOption.map(doc => {
        doc.get \ "age" mustEqual JInt(47)
      }).await

    }
  }
}
