package h8io.cfg.raw

import com.typesafe.config.*

import java.net.URL

package object hocon {
  def apply(urls: URL*): Node.Map[Id.Root] =
    MapImpl(Id.Root,
      urls.iterator
        .map(ConfigFactory.parseURL)
        .reduceOption((p, n) => n withFallback p)
        .map(ConfigFactory.load)
        .getOrElse(ConfigFactory.load).root())

  @inline private[hocon] def wrap[I <: Id](id: I, value: ConfigValue): Node[I] =
    value match {
      case obj: ConfigObject => MapImpl(id, obj)
      case list: ConfigList => SeqImpl(id, list)
      case scalar: ConfigValue => scalar.valueType match {
          case ConfigValueType.NULL => Node.Null(id, OriginImpl(scalar.origin))
          case _ => Node.Scalar(id, scalar.unwrapped.toString, OriginImpl(scalar.origin))
        }
    }
}
