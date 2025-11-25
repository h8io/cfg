package h8io.cfg.raw

import com.typesafe.config.*

import java.net.URL

package object hocon {
  def apply(urls: URL*): Node.IMap[Id.Root] =
    MapImpl(Id.Root,
      urls.iterator
        .map(ConfigFactory.parseURL)
        .reduceOption((p, n) => n withFallback p)
        .map(ConfigFactory.load)
        .getOrElse(ConfigFactory.load).root())

  @inline private[hocon] def wrap[I <: Id](id: I, value: ConfigValue): Node.ISome[I] =
    value match {
      case obj: ConfigObject => MapImpl(id, obj)
      case list: ConfigList => SeqImpl(id, list)
      case scalar: ConfigValue => scalar.valueType match {
          case ConfigValueType.NULL => Node.INull(id, LocationImpl(scalar.origin))
          case _ => Node.IScalar(id, scalar.unwrapped.toString, LocationImpl(scalar.origin))
        }
    }

  private[hocon] val RenderOptions: ConfigRenderOptions = ConfigRenderOptions.defaults().setJson(false)
}
