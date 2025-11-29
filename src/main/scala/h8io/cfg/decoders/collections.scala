package h8io.cfg.decoders

import cats.syntax.all.*
import h8io.cfg.raw.{Id, Node}
import h8io.cfg.{BaseDecoder, CfgValue, Decoder, ValuesOps}
import h8io.reflect.typeOf

import scala.collection.mutable

trait collections {
  implicit def VectorDecoder[T: Decoder]: Decoder[Vector[T]] =
    new BaseDecoder[Vector[T]] {
      override def parse(seq: Node.Seq): CfgValue[Vector[T]] =
        seq.iterator.map {
          case item: Node.Value => Decoder[T](item)
          case item: Node.INull[Id.Index] => item.invalid
        }.build[Vector[T], mutable.Builder[T, Vector[T]]](Vector.newBuilder[T])
    }

  implicit def MapDecoder[T: Decoder]: Decoder[Map[String, T]] =
    new BaseDecoder[Map[String, T]] {
      override def parse(map: Node.Map): CfgValue[Map[String, T]] =
        map.iterator.collect {
          case entry: Node.Value => Decoder[T](entry).map(value => entry.id.key -> value)
        }.build[Map[String, T], mutable.Builder[(String, T), Map[String, T]]](Map.newBuilder[String, T])
    }
}
