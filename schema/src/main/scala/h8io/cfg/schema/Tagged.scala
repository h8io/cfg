package h8io.cfg.schema

import cats.syntax.all.*
import h8io.cfg.Node
import h8io.cfg.schema.errors.{AmbiguousMap, NonScalarTag}

final case class Tagged(tag: String, node: Node.Some)

object Tagged {
  val decoder: Decoder[Tagged] = new SelectiveDecoder[Tagged] {
    override def parse(map: Node.Map): CfgValue[Tagged] =
      map.tag match {
        case Some(tag) => Tagged(tag, map).valid
        case None =>
          map("_") match {
            case scalar: Node.IScalar[?] => Tagged(scalar.value, map - "_").valid
            case _: Node.ISome[?] => NonScalarTag(map).invalid
            case _ =>
              if (map.size == 1) {
                val entry = map.iterator.next()
                Tagged(entry.id.key, entry).valid
              } else AmbiguousMap(map).invalid
          }
      }
  }
}
