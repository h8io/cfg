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
          case _ => Node.Scalar(id, scalar.unwrapped.toString, LocationImpl(scalar))
        }
    }

  @inline private def wrap[I <: Id](id: I, obj: ConfigObject): Node.IMap[I] = MapImpl(id, obj)

  private[hocon] val RenderOptions: ConfigRenderOptions = ConfigRenderOptions.defaults().setJson(false)
}
