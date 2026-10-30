package h8io.cfg.impl.hocon

import com.typesafe.config.{ConfigFactory, ConfigObject}

object context {

  /** No tags, as a stable identifier, so that a pattern can match it where `Set.empty` cannot stand. */
  val NoTags: Set[String] = Set.empty

  implicit final class CfgContext(private val sc: StringContext) extends AnyVal {
    def hocon(args: Any*): ConfigObject = ConfigFactory.parseString(sc.s(args*)).resolve().root
  }
}
