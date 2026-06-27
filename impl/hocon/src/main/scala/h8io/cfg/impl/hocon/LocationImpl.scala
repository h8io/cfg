package h8io.cfg.impl.hocon

import com.typesafe.config.{ConfigOrigin, ConfigValue}
import h8io.cfg.Location

private[hocon] final case class LocationImpl(origin: ConfigOrigin) extends Location {
  override def description: String = origin.description
}

private[hocon] object LocationImpl {
  def apply(value: ConfigValue): Location = LocationImpl(value.origin)
}
