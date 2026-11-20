package h8io.cfg.impl

import com.typesafe.config.*
import h8io.cfg.{Id, Node}

package object hocon {
  @inline private[hocon] def wrap[I <: Id](id: I, value: ConfigValue): Node.ISome[I] =
    value match {
      case obj: ConfigObject => wrap(id, obj)
      case list: ConfigList => SeqImpl(id, list)
      case scalar: ConfigValue => scalar.valueType match {
          case ConfigValueType.NULL => Node.Null(id, None, LocationImpl(scalar))
          case _ => Node.Scalar(id, None, scalar.unwrapped.toString, LocationImpl(scalar))
        }
    }

  @inline private[hocon] def wrap[I <: Id](id: I, obj: ConfigObject): Node.IMap[I] = MapImpl(id, obj)

  private[hocon] val RenderOptions: ConfigRenderOptions = ConfigRenderOptions.defaults().setJson(false)
}
