package h8io.cfg.raw.hocon

import com.typesafe.config.{ConfigList, ConfigObject, ConfigValue, ConfigValueType}
import h8io.cfg.raw.{Id, Node}

private[hocon] object Wrap {
  @inline def apply[I <: Id](id: I, value: ConfigValue): Node[I] =
    value match {
      case obj: ConfigObject => MapImpl(id, obj)
      case list: ConfigList => SeqImpl(id, list)
      case scalar: ConfigValue => scalar.valueType match {
          case ConfigValueType.NULL => Node.Null(id, OriginImpl(scalar.origin))
          case _ => Node.Scalar(id, scalar.unwrapped.toString, OriginImpl(scalar.origin))
        }
    }
}
