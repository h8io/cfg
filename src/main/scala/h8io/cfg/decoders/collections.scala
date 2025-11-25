package h8io.cfg.decoders

import cats.syntax.all.*
import h8io.cfg.raw.{Id, Node}
import h8io.cfg.{BaseDecoder, Decoder}
import h8io.reflect.typeOf

trait collections {
  def VectorDecoder[T: Decoder]: Decoder[Vector[T]] =
    new BaseDecoder[Vector[T]] {
      override def parse(seq: Node.Seq): Decoder.Result[Vector[T]] =
        seq.iterator.map {
          case item: Node.Value => Decoder[T](item)
          case item: Node.INull[Id.Index] => item.invalidNec
        }.toVector.sequence
    }

  def MapDecoder[T: Decoder]: Decoder[Map[String, T]] =
    new BaseDecoder[Map[String, T]] {
      override def parse(map: Node.Map): Decoder.Result[Map[String, T]] =
        map.iterator.map {
          case entry: Node.Value => Decoder[T](entry).map(value => entry.id.key -> value)
          case entry: Node.INull[Id.Key] => entry.invalidNec
        }.toVector.sequence.map(_.toMap)
    }
}
