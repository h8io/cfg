package h8io.cfg.impl

import com.typesafe.config.{ConfigList, ConfigObject, ConfigValue, ConfigValueType}
import h8io.cfg.{CfgNull, CfgScalar, CfgValue}

package object hocon {
  private[hocon] def wrap(value: ConfigValue): CfgValue =
    value match {
      case obj: ConfigObject => CfgMapImpl(obj)
      case list: ConfigList => CfgListImpl(list)
      case scalar: ConfigValue => scalar.valueType match {
          case ConfigValueType.NULL => CfgNull(CfgOriginImpl(scalar.origin))
          case _ => CfgScalar(scalar.unwrapped.toString, CfgOriginImpl(scalar.origin))
        }
    }
}
