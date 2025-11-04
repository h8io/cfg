package h8io.cfg.raw.hocon

import com.typesafe.config.{ConfigList, ConfigObject, ConfigValue, ConfigValueType}
import h8io.cfg.raw.{Entry, Path}

private[hocon] object Wrap {
  @inline def apply[P <: Path](path: P, value: ConfigValue): Entry[P] =
    value match {
      case obj: ConfigObject => MapImpl(path, obj)
      case list: ConfigList => SeqImpl(path, list)
      case scalar: ConfigValue => scalar.valueType match {
          case ConfigValueType.NULL => Entry.Null(path, OriginImpl(scalar.origin))
          case _ => Entry.Scalar(path, scalar.unwrapped.toString, OriginImpl(scalar.origin))
        }
    }
}
