package h8io.cfg.impl.yaml

import h8io.cfg.impl.yaml.context.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.snakeyaml.engine.v2.common.FlowStyle
import org.snakeyaml.engine.v2.exceptions.YamlEngineException
import org.snakeyaml.engine.v2.events.Event
import org.snakeyaml.engine.v2.nodes.{Node as YamlNode, SequenceNode, Tag}
import org.snakeyaml.engine.v2.parser.ParserImpl
import org.snakeyaml.engine.v2.scanner.StreamReader

import java.io.StringReader

import scala.jdk.CollectionConverters.*

class DepthTest extends AnyFlatSpec with Matchers {
  private def message(line: Int, column: Int): String =
    s"A configuration nests deeper than 256 levels at $Label: ${line.toString}:${column.toString}"

  /** `n` flow sequences, one in another. */
  private def nested(n: Int, inside: String = ""): String = "[" * n + inside + "]" * n

  "A document" should "nest 256 levels, the root's among them, and no more" in {
    parse("a: " + nested(255))
    the[YamlEngineException] thrownBy parse("a: " + nested(256)) should have message message(1, 4 + 255)
  }

  it should "fail before the composer recurses, however deep, flow or block, on a stack of 1 MB" in {
    val n = 100000
    val inputs = List(
      "a: " + nested(n),
      "a: " + "{a: " * n + "}" * n,
      // indented, a block's text grows with the square of its depth, past snakeyaml-engine's 3 million code points at 2 500 levels
      (0 until 2000).map(i => " " * i + "a:").mkString("\n"),
      (0 until 2000).map(i => " " * i + "-").mkString("\n")
    )
    var outcomes: List[String] = Nil
    val thread = new Thread(
      null,
      { () =>
        outcomes = inputs.map { text =>
          try { parse(text); "parsed" }
          catch {
            case e: YamlEngineException => e.getMessage.takeWhile(_ != ':')
            case _: StackOverflowError => "overflow"
          }
        }
      },
      "1 MB",
      1L << 20
    )
    thread.start()
    thread.join()
    outcomes shouldBe inputs.map(_ => "A configuration nests deeper than 256 levels at " + Label)
  }

  "Bounded" should "say whether the parser has events left, as the parser does" in {
    val parser =
      new Bounded(new ParserImpl(settings(Label), new StreamReader(settings(Label), new StringReader("a: 1"))))
    val events = Iterator.continually(parser).takeWhile(_.hasNext).map(_.next().getEventId).toList
    events.head shouldBe Event.ID.StreamStart
    events.last shouldBe Event.ID.StreamEnd
  }

  "An alias" should "not take the tree past the limit, reported where it stands" in {
    // the anchor 250 levels below the root's, used 10 levels down: 261 levels
    val anchored = "x: &x " + nested(250, "1")
    parse(anchored + "\ny: " + nested(5, "*x"))
    the[YamlEngineException] thrownBy parse(anchored + "\ny: " + nested(10, "*x")) should have message
      message(2, 4 + 9)
  }

  "validated" should "fail on a tree past the limit built other than by the composer" in {
    def sequence(children: YamlNode*): YamlNode =
      new SequenceNode(Tag.SEQ, children.toList.asJava, FlowStyle.FLOW)
    val deep = (1 to 257).foldLeft(sequence())((inner, _) => sequence(inner))
    the[YamlEngineException] thrownBy validated(deep) should have message
      "A configuration nests deeper than 256 levels at <unknown>"
  }
}
