package h8io.cfg.impl.yaml

import h8io.cfg.impl.yaml.context.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.snakeyaml.engine.v2.common.FlowStyle
import org.snakeyaml.engine.v2.events.{SequenceEndEvent, SequenceStartEvent}

import java.util.Optional

class WrittenTest extends AnyFlatSpec with Matchers {
  "Written" should "fail on events that do not follow the nodes" in {
    val events =
      Vector(new SequenceStartEvent(Optional.empty(), Optional.empty(), true, FlowStyle.FLOW), new SequenceEndEvent())
    an[IllegalStateException] should be thrownBy Written(list"[x]", events)
  }
}
