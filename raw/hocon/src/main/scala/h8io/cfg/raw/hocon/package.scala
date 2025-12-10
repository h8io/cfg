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

  @inline private[hocon] def wrap[I <: Id](id: I, value: ConfigValue): Node.ISome[I] =
    value match {
      case obj: ConfigObject => wrap(id, obj)
      case list: ConfigList => SeqImpl(id, list)
      case scalar: ConfigValue => scalar.valueType match {
          case ConfigValueType.NULL => Node.Null(id, LocationImpl(scalar))
          case _ =>
            val raw = scalar.unwrapped.toString
            val (tag, value) = raw.split("::", 2) match {
              case Array(tag, value) if tag.nonEmpty => (Tag.Some(tag, LocationImpl(scalar)), value)
              case noTag => (Tag.None(LocationImpl(scalar)), noTag.last)
            }
            Node.Scalar(id, value, tag, LocationImpl(scalar))
        }
    }

  @inline private[hocon] def TagKey = "_"

  @inline private def wrap[I <: Id](id: I, obj: ConfigObject): Node.IMap[I] =
    MapImpl(
      id,
      obj.withoutKey(TagKey),
      if (obj.containsKey(TagKey)) {
        obj.get(TagKey) match {
          case unsupported @ (_: ConfigObject | _: ConfigList) => UnsupportedTag(unsupported)
          case scalar: ConfigValue => scalar.valueType match {
              case ConfigValueType.NULL => Tag.None(LocationImpl(scalar))
              case _ =>
                val tag = scalar.unwrapped.toString
                if (tag.isEmpty) Tag.None(LocationImpl(scalar)) else Tag.Some(tag, LocationImpl(scalar))
            }
        }
      } else Tag.None(LocationImpl(obj))
    )

  private[hocon] val RenderOptions: ConfigRenderOptions = ConfigRenderOptions.defaults().setJson(false)
}
