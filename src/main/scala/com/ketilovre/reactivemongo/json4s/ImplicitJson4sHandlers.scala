package com.ketilovre.reactivemongo.json4s

import org.json4s._

trait ImplicitJson4sHandlers {

  implicit object JObjectReader extends Reader[JObject] {
    implicit val formats = org.json4s.DefaultFormats
    def read(json: JValue): JObject = json.extract[JObject]
  }

  implicit object JValueWriter extends Writer[JValue] {
    def write(json: JValue): JValue = json
  }
}

object ImplicitJson4sHandlers extends ImplicitJson4sHandlers
