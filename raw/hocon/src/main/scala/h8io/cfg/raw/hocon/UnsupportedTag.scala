package h8io.cfg.raw.hocon

import com.typesafe.config.ConfigValue
import h8io.cfg.raw.{Location, Tag}

private[hocon] final case class UnsupportedTag(value: ConfigValue) extends Tag.Error {
  def location: Location = LocationImpl(value)
}
