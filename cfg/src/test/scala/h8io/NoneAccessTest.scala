package h8io

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Outside `h8io.cfg`, so that `private[cfg]` applies: only a container may create a `Node.INone`. */
class NoneAccessTest extends AnyFlatSpec with Matchers {
  "INone" should "not be constructible outside cfg" in {
    assertCompiles("""(none: cfg.Node.INone[cfg.Id.Key]) => cfg.Node.INone.unapply(none)""")
    assertTypeError("""(parent: cfg.Node.Map) => cfg.Node.INone(cfg.Id.Key("x", cfg.Id.Root), parent)""")
    assertTypeError("""(parent: cfg.Node.Map) => cfg.Node.None(cfg.Id.Key("x", cfg.Id.Root), parent)""")
    assertTypeError("""(parent: cfg.Node.Map) => new cfg.Node.INone(cfg.Id.Key("x", cfg.Id.Root), parent)""")
    assertTypeError("""(none: cfg.Node.INone[cfg.Id.Key]) => none.copy(id = cfg.Id.Key("y", cfg.Id.Root))""")
  }
}
