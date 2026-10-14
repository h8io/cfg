package h8io.cfg.schema.decoders

import cats.syntax.all.*
import h8io.cfg.schema.errors.UnexpectedNode
import h8io.cfg.schema.{Decoder, ValuesOps}
import h8io.cfg.{Id, Node}
import izumi.reflect.Tag

import scala.collection.mutable

trait collections {
  implicit def vectorDecoder[T: Decoder: Tag]: Decoder[Vector[T]] = {
    case seq: Node.Seq =>
      seq.iterator.map {
        case item: Node.Value => Decoder[T](item)
        case item: Node.INull[Id.Index] => item.invalid
      }.collectInto[Vector[T], mutable.Builder[T, Vector[T]]](Vector.newBuilder[T])
    case node =>
      UnexpectedNode[Vector[T]](node).invalid
  }

  implicit def mapDecoder[T: Decoder: Tag]: Decoder[Map[String, T]] = {
    case map: Node.Map =>
      map.iterator.collect {
        case entry: Node.Value => Decoder[T](entry).map(value => entry.id.key -> value)
      }.collectInto[Map[String, T], mutable.Builder[(String, T), Map[String, T]]](Map.newBuilder[String, T])
    case node =>
      UnexpectedNode[Map[String, T]](node).invalid
  }
}
