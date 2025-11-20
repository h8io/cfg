package h8io.cfg.raw.hocon

import com.typesafe.config.ConfigOrigin
import h8io.cfg.raw.Location

private[hocon] final case class LocationImpl(origin: ConfigOrigin) extends Location {
  def description: String = origin.description
}
