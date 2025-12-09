package h8io.cfg.raw

import com.typesafe.config.*

import java.net.URL

package object hocon {
  def apply(urls: URL*): Node.IMap[Id.Root] =
    wrap(Id.Root,
      urls.iterator
        .map(ConfigFactory.parseURL)
        .reduceOption((p, n) => n withFallback p)
        .map(ConfigFactory.load)
        .getOrElse(ConfigFactory.load).root())

  private def TagKey = "_"

  @inline private[hocon] def wrap[I <: Id](id: I, value: ConfigValue): Node.ISome[I] =
    value match {
      case obj: ConfigObject => wrap(id, obj)
      case list: ConfigList => SeqImpl(id, list)
      case scalar: ConfigValue => scalar.valueType match {
          case ConfigValueType.NULL => Node.Null(id, LocationImpl(scalar.origin))
          case _ =>
            val raw = scalar.unwrapped.toString
            val (tag, value) = raw.split("::", 2) match {
              case Array(tag, value) if tag.nonEmpty => (Some(tag), value)
              case noTag => (None, noTag.last)
            }
            Node.Scalar(id, value, tag, LocationImpl(scalar.origin))
        }
    }

  @inline private def wrap[I <: Id](id: I, obj: ConfigObject): Node.IMap[I] =
    MapImpl(id, obj.withoutKey(TagKey),
      if (obj.containsKey(TagKey)) Option(obj.get(TagKey).unwrapped()).map(_.toString).filter(_.nonEmpty) else None)

  private[hocon] val RenderOptions: ConfigRenderOptions = ConfigRenderOptions.defaults().setJson(false)
}
