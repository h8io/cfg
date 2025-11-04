package h8io.cfg.impl.hocon

import com.typesafe.config.ConfigOrigin
import h8io.cfg.Origin

private[hocon] final case class OriginImpl(underlying: ConfigOrigin) extends Origin {
  def description: String = underlying.description
}
