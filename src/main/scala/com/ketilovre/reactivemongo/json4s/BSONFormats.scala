package com.ketilovre.reactivemongo.json4s

import org.json4s._
import reactivemongo.bson._
import reactivemongo.bson.utils.Converters

/**
 * Json4s Formats for BSONValues.
 */
object BSONFormats {

  implicit val formats = org.json4s.DefaultFormats

  trait PartialFormat[T <: BSONValue] extends JsonFormat[T] {
    def partialReads: PartialFunction[JValue, Option[T]]
    def partialWrites: PartialFunction[BSONValue, JValue]

    def write(t: T): JObject = partialWrites(t).extract[JObject]
    def read(json: JValue) = partialReads.lift(json).flatten.getOrElse(throw new RuntimeException(s"unhandled $json"))
  }

  implicit object BSONDoubleFormat extends PartialFormat[BSONDouble] {
    val partialReads: PartialFunction[JValue, Option[BSONDouble]] = {
      case JDouble(f)                                     => Some(BSONDouble(f.toDouble))
      case JDecimal(f)                                    => Some(BSONDouble(f.toDouble))
      case JObject(JField("$double", JDouble(v)) +: Nil)  => Some(BSONDouble(v.toDouble))
      case JObject(JField("$double", JDecimal(v)) +: Nil) => Some(BSONDouble(v.toDouble))
    }
    val partialWrites: PartialFunction[BSONValue, JValue] = {
      case double: BSONDouble => JDouble(double.value)
    }
  }

  implicit object BSONStringFormat extends PartialFormat[BSONString] {
    def partialReads: PartialFunction[JValue, Option[BSONString]] = {
      case JString(str) => Some(BSONString(str))
    }
    val partialWrites: PartialFunction[BSONValue, JValue] = {
      case str: BSONString => JString(str.value)
    }
  }

  class BSONDocumentFormat(toBSON: JValue => Option[BSONValue], toJSON: BSONValue => JValue) extends PartialFormat[BSONDocument] {
    def partialReads: PartialFunction[JValue, Option[BSONDocument]] = {
      case obj: JObject =>
        try {
          Some(BSONDocument(obj.obj.map { tuple =>
            tuple._1 -> (toBSON(tuple._2) match {
              case Some(bson) => bson
              case None       => throw new RuntimeException("Unhandled Json value")
            })
          }))
        } catch {
          case e: Throwable => None
        }
    }
    val partialWrites: PartialFunction[BSONValue, JValue] = {
      case doc: BSONDocument => JObject(doc.elements.toList.map { elem =>
        JField(elem._1, toJSON(elem._2))
      })
    }
  }

  implicit object BSONDocumentFormat extends BSONDocumentFormat(toBSON, toJSON)

  class BSONArrayFormat(toBSON: JValue => Option[BSONValue], toJSON: BSONValue => JValue) extends PartialFormat[BSONArray] {
    def partialReads: PartialFunction[JValue, Option[BSONArray]] = {
      case arr: JArray =>
        try {
          Some(BSONArray(arr.arr.map { value =>
            toBSON(value) match {
              case Some(bson) => bson
              case None       => throw new RuntimeException("Unhandled Json value")
            }
          }))
        } catch {
          case e: Throwable => None
        }
    }
    def partialWrites: PartialFunction[BSONValue, JValue] = {
      case array: BSONArray => {
        JArray(array.values.toList.map { value =>
          toJSON(value)
        })
      }
    }
  }

  implicit object BSONArrayFormat extends BSONArrayFormat(toBSON, toJSON)

  implicit object BSONObjectIDFormat extends PartialFormat[BSONObjectID] {
    def partialReads: PartialFunction[JValue, Option[BSONObjectID]] = {
      case JObject(("$oid", JString(v)) +: Nil) => Some(BSONObjectID(v))
    }
    val partialWrites: PartialFunction[BSONValue, JValue] = {
      case oid: BSONObjectID => JObject(JField("$oid", JString(oid.stringify)))
    }
  }

  implicit object BSONBooleanFormat extends PartialFormat[BSONBoolean] {
    def partialReads: PartialFunction[JValue, Option[BSONBoolean]] = {
      case JBool(v) => Some(BSONBoolean(v))
    }
    val partialWrites: PartialFunction[BSONValue, JValue] = {
      case boolean: BSONBoolean => JBool(boolean.value)
    }
  }

  implicit object BSONDateTimeFormat extends PartialFormat[BSONDateTime] {
    def partialReads: PartialFunction[JValue, Option[BSONDateTime]] = {
      case JObject(("$date", JInt(v)) +: Nil) => Some(BSONDateTime(v.toLong))
      case JObject(("$date", JDouble(v)) +: Nil) => Some(BSONDateTime(v.toLong))
      case JObject(("$date", JDecimal(v)) +: Nil) => Some(BSONDateTime(v.toLong))
    }
    val partialWrites: PartialFunction[BSONValue, JValue] = {
      case dt: BSONDateTime => JObject(JField("$date", JInt(dt.value)))
    }
  }

  implicit object BSONTimestampFormat extends PartialFormat[BSONTimestamp] {
    def partialReads: PartialFunction[JValue, Option[BSONTimestamp]] = {
      case JObject(("$time", JInt(v)) +: Nil) => Some(BSONTimestamp(v.toLong))
      case JObject(("$time", JDouble(v)) +: Nil) => Some(BSONTimestamp(v.toLong))
      case JObject(("$time", JDecimal(v)) +: Nil) => Some(BSONTimestamp(v.toLong))
    }
    val partialWrites: PartialFunction[BSONValue, JValue] = {
      case ts: BSONTimestamp => JObject(JField("$time", JInt(ts.value.toInt)), JField("i", JInt(ts.value >>> 4)))
    }
  }

  implicit object BSONRegexFormat extends PartialFormat[BSONRegex] {
    def partialReads: PartialFunction[JValue, Option[BSONRegex]] = {
      case js: JObject if js.obj.size == 1 && js.obj.head._1 == "$regex" =>
        js.obj.head._2.extractOpt[String].map(rx => Some(BSONRegex(rx, ""))).getOrElse(None)
      case js: JObject if js.obj.size == 2 && js.obj.exists(_._1 == "$regex") && js.obj.exists(_._1 == "$options") =>
        val rx = (js \ "$regex").extractOpt[String]
        val opts = (js \ "$options").extractOpt[String]
        (rx, opts) match {
          case (Some(rx), Some(opts)) => Some(BSONRegex(rx, opts))
          case _                      => None
        }
    }
    val partialWrites: PartialFunction[BSONValue, JValue] = {
      case rx: BSONRegex =>
        if (rx.flags.isEmpty)
          JObject(JField("$regex", JString(rx.value)))
        else JObject(JField("$regex", JString(rx.value)), JField("$options", JString(rx.flags)))
    }
  }

  implicit object BSONNullFormat extends PartialFormat[BSONNull.type] {
    def partialReads: PartialFunction[JValue, Option[BSONNull.type]] = {
      case JNull => Some(BSONNull)
    }
    val partialWrites: PartialFunction[BSONValue, JValue] = {
      case BSONNull => JNull
    }
  }

  implicit object BSONUndefinedFormat extends PartialFormat[BSONUndefined.type] {
    def partialReads: PartialFunction[JValue, Option[BSONUndefined.type]] = {
      case JNothing => Some(BSONUndefined)
    }
    val partialWrites: PartialFunction[BSONValue, JValue] = {
      case BSONUndefined => JNothing
    }
  }

  implicit object BSONIntegerFormat extends PartialFormat[BSONInteger] {
    def partialReads: PartialFunction[JValue, Option[BSONInteger]] = {
      case JObject(("$int", JInt(i)) +: Nil) => Some(BSONInteger(i.toInt))
      case JInt(i)                           => Some(BSONInteger(i.toInt))
    }
    val partialWrites: PartialFunction[BSONValue, JValue] = {
      case int: BSONInteger => JInt(int.value)
    }
  }

  implicit object BSONLongFormat extends PartialFormat[BSONLong] {
    def partialReads: PartialFunction[JValue, Some[BSONLong]] = {
      case JObject(("$long", JInt(long)) +: Nil) => Some(BSONLong(long.toLong))
      case JInt(long)                            => Some(BSONLong(long.toLong))
    }
    val partialWrites: PartialFunction[BSONValue, JValue] = {
      case long: BSONLong => JInt(long.value)
    }
  }

  implicit object BSONBinaryFormat extends PartialFormat[BSONBinary] {
    def partialReads: PartialFunction[JValue, Option[BSONBinary]] = {
      case JString(str) => try {
        Some(BSONBinary(Converters.str2Hex(str), Subtype.UserDefinedSubtype))
      } catch {
        case e: Throwable => None
      }
      case obj: JObject if obj.obj.exists {
        case (str, _: JString) if str == "$binary" => true
        case _                                     => false
      } => try {
        Some(BSONBinary(Converters.str2Hex((obj \ "$binary").extract[String]), Subtype.UserDefinedSubtype))
      } catch {
        case e: Throwable => None
      }
    }
    val partialWrites: PartialFunction[BSONValue, JValue] = {
      case binary: BSONBinary =>
        val remaining = binary.value.readable()
        JObject(
          JField("$binary", JString(Converters.hex2Str(binary.value.slice(remaining).readArray(remaining)))),
          JField("$type", JString(Converters.hex2Str(Array(binary.subtype.value.toByte)))))

    }
  }

  implicit object BSONSymbolFormat extends PartialFormat[BSONSymbol] {
    def partialReads: PartialFunction[JValue, Option[BSONSymbol]] = {
      case JObject(("$symbol", JString(v)) +: Nil) => Some(BSONSymbol(v))
    }
    val partialWrites: PartialFunction[BSONValue, JValue] = {
      case BSONSymbol(s) => JObject(JField("$symbol", JString(s)))
    }
  }

  def toBSON(json: JValue): Option[BSONValue] = {
    BSONStringFormat.partialReads.
    orElse(BSONObjectIDFormat.partialReads).
    orElse(BSONDateTimeFormat.partialReads).
    orElse(BSONTimestampFormat.partialReads).
    orElse(BSONBinaryFormat.partialReads).
    orElse(BSONRegexFormat.partialReads).
    orElse(BSONDoubleFormat.partialReads).
    orElse(BSONIntegerFormat.partialReads).
    orElse(BSONLongFormat.partialReads).
    orElse(BSONBooleanFormat.partialReads).
    orElse(BSONNullFormat.partialReads).
    orElse(BSONUndefinedFormat.partialReads).
    orElse(BSONSymbolFormat.partialReads).
    orElse(BSONArrayFormat.partialReads).
    orElse(BSONDocumentFormat.partialReads).
    lift(json).getOrElse(None)
  }

  def toJSON(bson: BSONValue): JValue =
    BSONObjectIDFormat.partialWrites.
    orElse(BSONDateTimeFormat.partialWrites).
    orElse(BSONTimestampFormat.partialWrites).
    orElse(BSONBinaryFormat.partialWrites).
    orElse(BSONRegexFormat.partialWrites).
    orElse(BSONDoubleFormat.partialWrites).
    orElse(BSONIntegerFormat.partialWrites).
    orElse(BSONLongFormat.partialWrites).
    orElse(BSONBooleanFormat.partialWrites).
    orElse(BSONNullFormat.partialWrites).
    orElse(BSONUndefinedFormat.partialWrites).
    orElse(BSONStringFormat.partialWrites).
    orElse(BSONSymbolFormat.partialWrites).
    orElse(BSONArrayFormat.partialWrites).
    orElse(BSONDocumentFormat.partialWrites).
    lift(bson).getOrElse(throw new RuntimeException(s"unhandled json value: $bson"))
}
