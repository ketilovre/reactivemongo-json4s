import org.json4s._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class ConversionSpec extends Specification with ScalaCheck {

  "Converters" should {

    import com.ketilovre.reactivemongo.json4s.BSONFormats._
    import helpers.JsonGen._

    "convert JValues to BSON and back without losing anything" ! prop { (json: JValue) =>

      toJSON(toBSON(json).get) mustEqual json

    }.set(minTestsOk = 1000, workers = 5)
  }
}
