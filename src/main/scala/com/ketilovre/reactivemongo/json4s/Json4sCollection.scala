package com.ketilovre.reactivemongo.json4s

import org.json4s._
import org.json4s.JsonDSL._
import reactivemongo.api._
import reactivemongo.api.collections._
import reactivemongo.bson.BSONDocument
import reactivemongo.bson.buffer._
import reactivemongo.core.commands.{ GetLastError, LastError }
import scala.concurrent.{ ExecutionContext, Future }

/**
 * A Collection that interacts with the Play JSON library, using `Reads` and `Writes`.
 */
object `package` {
  implicit object Json4sCollectionProducer extends GenericCollectionProducer[JObject, Reader, Writer, Json4sCollection] {
    def apply(db: DB, name: String, failoverStrategy: FailoverStrategy) = new Json4sCollection(db, name, failoverStrategy)
  }
}

trait Json4sGenericHandlers extends GenericHandlers[JObject, Reader, Writer] {

  import com.ketilovre.reactivemongo.json4s.BSONFormats._

  implicit val formats = org.json4s.DefaultFormats

  object StructureBufferReader extends BufferReader[JObject] {
    def read(buffer: ReadableBuffer) = {
      BSONDocumentFormat.write(BSONDocument.read(buffer))
    }
  }
  object StructureBufferWriter extends BufferWriter[JObject] {
    def write[B <: reactivemongo.bson.buffer.WritableBuffer](document: JObject, buffer: B): B = {
      BSONDocument.write(document.as[BSONDocument], buffer)
      buffer
    }
  }
  case class BSONStructureReader[T](reader: Reader[T]) extends GenericReader[JObject, T] {
    def read(doc: JObject) = reader.read(doc)
  }
  case class BSONStructureWriter[T](writer: Writer[T]) extends GenericWriter[T, JObject] {
    def write(t: T) = writer.write(t).extract[JObject]
  }
  def StructureReader[T](reader: Reader[T]) = BSONStructureReader(reader)
  def StructureWriter[T](writer: Writer[T]): GenericWriter[T, JObject] = BSONStructureWriter(writer)
}

object Json4sGenericHandlers extends Json4sGenericHandlers

case class JSONDocumentReaderAsBufferReader[T](reader: Reader[T]) extends BufferReader[T] {
  def read(buffer: ReadableBuffer) = reader.read(Json4sGenericHandlers.StructureBufferReader.read(buffer))
}

/**
 * A Collection that interacts with the Play JSON library, using `Reads` and `Writes`.
 */
case class Json4sCollection(
                           db: DB,
                           name: String,
                           failoverStrategy: FailoverStrategy) extends GenericCollection[JObject, Reader, Writer] with Json4sGenericHandlers with CollectionMetaCommands {

  def genericQueryBuilder: GenericQueryBuilder[JObject, Reader, Writer] =
    Json4sQueryBuilder(this, failoverStrategy)

  /**
   * Inserts the document, or updates it if it already exists in the collection.
   *
   * @param doc The document to save.
   */
  def save(doc: JObject)(implicit ec: ExecutionContext): Future[LastError] =
    save(doc, GetLastError())

  /**
   * Inserts the document, or updates it if it already exists in the collection.
   *
   * @param doc The document to save.
   * @param writeConcern the command message to send in order to control how the document is inserted. Defaults to GetLastError().
   */
  def save(doc: JObject, writeConcern: GetLastError)(implicit ec: ExecutionContext): Future[LastError] = {
    import reactivemongo.bson.BSONObjectID
    import com.ketilovre.reactivemongo.json4s.ImplicitJson4sHandlers._
    doc \ "_id" match {
      case JNothing => insert(doc merge JObject(JField("_id", BSONFormats.BSONObjectIDFormat.write(BSONObjectID.generate))), writeConcern)
      case id       => update(JObject(JField("_id", id)), doc, writeConcern, upsert = true)
    }
  }

  /**
   * Inserts the document, or updates it if it already exists in the collection.
   *
   * @param doc The document to save.
   * @param writeConcern the command message to send in order to control how the document is inserted. Defaults to GetLastError().
   */
  def save[T](doc: T, writeConcern: GetLastError = GetLastError())(implicit ec: ExecutionContext, writer: Writer[T]): Future[LastError] =
    save(writer.write(doc).extract[JObject], writeConcern)
}

case class Json4sQueryBuilder(
                             collection: Collection,
                             failover: FailoverStrategy,
                             queryOption: Option[JObject] = None,
                             sortOption: Option[JObject] = None,
                             projectionOption: Option[JObject] = None,
                             hintOption: Option[JObject] = None,
                             explainFlag: Boolean = false,
                             snapshotFlag: Boolean = false,
                             commentString: Option[String] = None,
                             options: QueryOpts = QueryOpts()) extends GenericQueryBuilder[JObject, Reader, Writer] with Json4sGenericHandlers {
  import reactivemongo.utils.option
  type Self = Json4sQueryBuilder

  private def empty = JObject()

  protected def writeStructureIntoBuffer[B <: WritableBuffer](document: JObject, buffer: B): B = {
    Json4sGenericHandlers.StructureBufferWriter.write(document, buffer)
  }

  object structureReader extends Reader[JObject] {
    def read(value: JValue): JObject = value.extract[JObject]
  }

  protected def toStructure[T](writer: Writer[T], subject: T) = writer.write(subject)

  def convert[T](reader: Reader[T]): BufferReader[T] = JSONDocumentReaderAsBufferReader(reader)

  def copy(queryOption: Option[JObject], sortOption: Option[JObject], projectionOption: Option[JObject], hintOption: Option[JObject], explainFlag: Boolean, snapshotFlag: Boolean, commentString: Option[String], options: QueryOpts, failover: FailoverStrategy): Json4sQueryBuilder =
    Json4sQueryBuilder(collection, failover, queryOption, sortOption, projectionOption, hintOption, explainFlag, snapshotFlag, commentString, options)

  def merge: JObject = {
    if (!sortOption.isDefined && !hintOption.isDefined && !explainFlag && !snapshotFlag && !commentString.isDefined)
      queryOption.getOrElse(empty)
    else {
      ("$query" -> queryOption.getOrElse(empty)) ~
      sortOption.map(o => JObject(JField("$orderby", o))).getOrElse(empty) ~
      hintOption.map(o => JObject(JField("$hint", o))).getOrElse(empty) ~
      commentString.map(o => JObject(JField("$comment", o))).getOrElse(empty) ~
      option(explainFlag, JBool(value = true)).map(o => JObject(JField("$explain", o))).getOrElse(empty) ~
      option(snapshotFlag, JBool(value = true)).map(o => JObject(JField("$snapshot", o))).getOrElse(empty)
    }
  }
}