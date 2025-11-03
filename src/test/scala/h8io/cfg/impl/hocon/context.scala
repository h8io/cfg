package h8io.cfg.impl.hocon

import com.typesafe.config.{ConfigFactory, ConfigObject}

object context {
  implicit final class CfgContext(private val sc: StringContext) extends AnyVal {
    def hocon(args: Any*): ConfigObject = ConfigFactory.parseString(sc.s(args*)).resolve().root
  }
}
