package h8io.cfg.impl.hocon

import com.typesafe.config.{ConfigList, ConfigObject, ConfigValue, ConfigValueType}
import h8io.cfg.{CfgNull, CfgScalar, CfgValue}

object Wrap {
  @inline private[hocon] def apply(value: ConfigValue): CfgValue =
    value match {
      case obj: ConfigObject => CfgMapImpl(obj)
      case list: ConfigList => CfgSeqImpl(list)
      case scalar: ConfigValue => scalar.valueType match {
          case ConfigValueType.NULL => CfgNull(CfgOriginImpl(scalar.origin))
          case _ => CfgScalar(scalar.unwrapped.toString, CfgOriginImpl(scalar.origin))
        }
    }
}
