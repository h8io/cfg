package h8io.cfg.impl.hocon

import com.typesafe.config.ConfigOrigin
import h8io.cfg.CfgOrigin

private[hocon] final case class CfgOriginImpl(underlying: ConfigOrigin) extends CfgOrigin {
  def description: String = underlying.description
}
