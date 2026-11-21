package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}
import h8io.cfg.impl.psi.Registering.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.IOException
import java.net.{URI, URL}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

class DirectiveTest extends AnyFlatSpec with Matchers {
  private def at(line: Int, column: Int, source: String = "s"): SourceLocation = SourceLocation(source, line, column)

  private def directive(
      directiveName: String,
      params: Parameter*)(
      body: (Node.IMap[Id.Root], DirectiveContext) => Either[DirectiveError, Fragment]): (String, Directive) =
    directiveName -> new Directive {
      def parameters: Seq[Parameter] = params
      def apply(arguments: Node.IMap[Id.Root], context: DirectiveContext): Either[DirectiveError, Fragment] =
        body(arguments, context)
    }

  private def loader(directives: (String, Directive)*): PsiLoader =
    Psi.builder.withDirectives(directives*).withSystemProperties(Map.empty).build.fold(e => fail(e.toString), identity)

  private def loaded(loader: PsiLoader, text: String): Node.IMap[Id.Root] =
    loader.parse("s", text).fold(errors => fail(errors.toString), identity)

  private def errors(loader: PsiLoader, text: String): List[PsiError] =
    loader.parse("s", text).fold(_.errors, root => fail(root.toString))

  private def keyed(names: String*): Id.Key =
    names.tail.foldLeft(Id.Key(names.head, Id.Root))((parent, name) => Id.Key(name, parent))

  private def node(text: String): Node.IMap[Id.Root] =
    Psi.parse("y", text).fold(errors => fail(errors.toString), identity)

  private def constant(name: String, fragment: => Fragment): (String, Directive) =
    directive(name)((_, _) => Right(fragment))

  private def nothing: (String, Directive) = directive("none")((_, _) => Right(Fragment.None))

  "A builder" should "report every error of its configuration at once" in {
    val x =
      directive("x", Parameter("p", required = true), Parameter("p", required = false))((_, _) => Right(Fragment.None))
    Psi.builder
      .withDirectives(directive("include")((_, _) => Right(Fragment.None)), x)
      .withDirectives(x, directive("y")((_, _) => Right(Fragment.None)))
      .disableDirective("y")
      .build shouldBe
      Left(
        PsiBuildErrors(
          ::(
            DuplicateDirective("include"),
            List(
              DuplicateParameter("x", "p"),
              DuplicateDirective("x"),
              DuplicateParameter("x", "p"),
              DisabledDirectiveRegistered("y"))
          )))
  }

  it should "give #prop and #env the maps received, which override nothing of their own" in {
    val built = Psi.builder
      .withSystemProperties(Map("a" -> "2"))
      .withEnvironment(Map("E" -> "3"))
      .build
      .fold(e => fail(e.toString), identity)
    val root = loaded(built, "a: 1\nb: #prop(a)\nc: #env(E)")
    root("a") shouldBe Node.IScalar(keyed("a"), None, "1", at(1, 4))
    root("b") shouldBe
      Node.IScalar(keyed("b"), None, "2", DirectiveLocation(at(2, 4), "prop", SystemPropertyLocation("a")))
    root("c") shouldBe
      Node.IScalar(keyed("c"), None, "3", DirectiveLocation(at(3, 4), "env", EnvironmentLocation("E")))
  }

  "A call" should "be reported when its directive is unknown or disabled" in {
    val disabling =
      Psi.builder.disableDirective("include").disableDirective("off").build.fold(e => fail(e.toString), identity)
    errors(disabling, "#include\na: #off\nb: #x") shouldBe
      List(DisabledDirective(at(1, 1), "include"), DisabledDirective(at(2, 4), "off"), UnknownDirective(at(3, 4), "x"))
    errors(loader(), "#include(file: \"x\")") shouldBe List(DirectiveFailed(at(1, 1), "include", IncludeNotFound("x")))
  }

  it should "give its arguments by parameter, in the order the parameters are declared" in {
    var received: Option[Node.IMap[Id.Root]] = None
    val d = directive("d", Parameter("a", required = true), Parameter("b", required = false), Parameter("c", false)) {
      (arguments, _) =>
        received = Some(arguments)
        Right(Fragment.None)
    }
    loaded(loader(d), "#d(c: 3, a: 1)").isEmpty shouldBe true
    received.map(_.toList) shouldBe
      Some(
        List(
          Node.IScalar(keyed("a"), None, "1", at(1, 13)),
          Node.IScalar(keyed("c"), None, "3", at(1, 7))))
    loaded(loader(d), "#d(1, 2)").isEmpty shouldBe true
    received.map(_.toList.map(_.id)) shouldBe Some(List(keyed("a"), keyed("b")))
    received.map(_.location) shouldBe Some(at(1, 1))
  }

  it should "not run with an argument to no parameter, received twice, or a required one missing" in {
    var runs = 0
    val d = directive("d", Parameter("a", required = true), Parameter("b", required = false)) { (_, _) =>
      runs += 1
      Right(Fragment.None)
    }
    errors(loader(d), "#d(1, 2, 3)\n#d(1, x: 2)\n#d(1, a: 2)\n#d(b: 1)") shouldBe
      List(
        ExtraArgument(at(1, 10)),
        UnknownArgument(at(2, 7), "x"),
        DuplicateArgument(at(3, 7), "a"),
        MissingArgument(at(4, 1), "a"))
    runs shouldBe 0
  }

  it should "report each required argument missing, their names told apart only past what a message quotes" in {
    val (primary, secondary) = ("connection_configuration_primary", "connection_configuration_secondary")
    val d = directive("d", Parameter(primary, required = true), Parameter(secondary, required = true))((_, _) =>
      Right(Fragment.None))
    errors(loader(d), "#d") shouldBe List(MissingArgument(at(1, 1), primary), MissingArgument(at(1, 1), secondary))
  }

  it should "fail with its handler's message, contributing nothing" in {
    errors(loader(directive("d")((_, _) => Left(Problem("no")))), "a: #d\nb: 1") shouldBe
      List(DirectiveFailed(at(1, 4), "d", Problem("no")))
  }

  it should "give nothing: no field in statement position, the field or element dropped in value position" in {
    val root = loaded(loader(nothing), "#none\na: #none\nb: [1, #none, 2]")
    root.toList.map(_.id) shouldBe List(keyed("b"))
    root("b") match {
      case seq: Node.ISeq[?] => seq.size shouldBe 2
      case other => fail(other.toString)
    }
  }

  "A fragment of a Node" should "keep its locations behind the call, and its tag unless one is written" in {
    val scalar = Node.IScalar(Id.Root, Some("t"), "v", SourceLocation("y", 3, 8))
    val root = loaded(loader(constant("k", Fragment(scalar))), "a: @u #k\nb: #k")
    root("a") shouldBe
      Node.IScalar(keyed("a"), Some("u"), "v", DirectiveLocation(at(1, 7), "k", SourceLocation("y", 3, 8)))
    root("b") shouldBe
      Node.IScalar(keyed("b"), Some("t"), "v", DirectiveLocation(at(2, 4), "k", SourceLocation("y", 3, 8)))
  }

  it should "merge as a map in statement position, overridden by the fields below it" in {
    val root = loaded(loader(constant("m", Fragment(node("x: 1\ny: [2, null]\nz { w: 3 }")))), "x: 0\n#m\nz { v: 4 }")
    def from(line: Int, column: Int) = DirectiveLocation(at(2, 1), "m", SourceLocation("y", line, column))
    root("x") shouldBe Node.IScalar(keyed("x"), None, "1", from(1, 4))
    root("y") match {
      case seq: Node.ISeq[?] =>
        seq.location shouldBe from(2, 4)
        seq.toList shouldBe
          List(
            Node.IScalar(Id.Index(0, keyed("y")), None, "2", from(2, 5)),
            Node.INull(Id.Index(1, keyed("y")), None, from(2, 8)))
      case other => fail(other.toString)
    }
    root("z") match {
      case map: Node.IMap[?] =>
        map.location shouldBe MergedLocation(at(3, 3), ::(from(3, 3), Nil))
        map.toList.map(_.id) shouldBe List(keyed("z", "w"), keyed("z", "v"))
      case other => fail(other.toString)
    }
  }

  it should "stand as a map in value position" in {
    loaded(loader(constant("m", Fragment(node("x: 1")))), "a: @t #m")("a") match {
      case map: Node.IMap[?] =>
        map.tag shouldBe Some("t")
        map.location shouldBe DirectiveLocation(at(1, 7), "m", SourceLocation("y", 1, 1))
        map("x") shouldBe
          Node.IScalar(keyed("a", "x"), None, "1", DirectiveLocation(at(1, 7), "m", SourceLocation("y", 1, 4)))
      case other => fail(other.toString)
    }
  }

  it should "not stand in statement position unless it is a map" in {
    val scalar = Node.IScalar(Id.Root, None, "v", SourceLocation("y", 1, 1))
    errors(loader(constant("k", Fragment(scalar))), "#k") shouldBe List(DirectiveNotMap(at(1, 1), "k"))
  }

  "A fragment parsed" should "expand its own directives and merge as written in statement position" in {
    val parsing = directive("p", Parameter("text", required = true)) { (arguments, context) =>
      arguments("text") match {
        case Node.IScalar(_, _, text, _) => Right(context.parse("t", text))
        case other => Left(Problem(other.toString))
      }
    }
    val root = loaded(loader(parsing, constant("k", Fragment(Node.IScalar(Id.Root, None, "v", at(9, 9))))),
      "#p(text: \"a: 1\\nb: #k\")\nc: @x #p(\"d: 2\")")
    root("a") shouldBe Node.IScalar(keyed("a"), None, "1", at(1, 4, "t"))
    root("b") shouldBe Node.IScalar(keyed("b"), None, "v", DirectiveLocation(at(2, 4, "t"), "k", at(9, 9)))
    root("c") match {
      case map: Node.IMap[?] =>
        map.tag shouldBe Some("x")
        map.location shouldBe at(1, 1, "t")
        map("d") shouldBe Node.IScalar(keyed("c", "d"), None, "2", at(1, 4, "t"))
      case other => fail(other.toString)
    }
  }

  it should "report the errors in it with every source's" in {
    val bad = directive("bad")((_, context) => Right(context.parse("t", "a b")))
    errors(loader(bad), "#bad\nc: ${d}") shouldBe List(ExpectedColon(at(1, 3, "t"), "'b'"))
  }

  it should "carry its errors, reported wherever it is used, once per load, and not where it is not" in {
    var kept: Option[Fragment] = None
    val cache = directive("cache") { (_, context) =>
      Right(kept.getOrElse {
        val fragment = context.parse("child", "good: 1\nbroken value")
        kept = Some(fragment)
        fragment
      })
    }
    errors(loader(cache), "#cache") shouldBe List(ExpectedColon(at(2, 8, "child"), "'value'"))
    errors(loader(cache), "#cache\na: #cache") shouldBe List(ExpectedColon(at(2, 8, "child"), "'value'"))
    val dropped = directive("dropped") { (_, context) =>
      val _ = context.parse("child", "broken value")
      Right(Fragment.None)
    }
    loaded(loader(dropped), "#dropped").isEmpty shouldBe true
  }

  it should "be a cycle when its source is being expanded, named by the chain" in {
    val self = directive("self")((_, context) => Right(context.parse("again", "#self")))
    errors(loader(self), "#self") shouldBe List(DirectiveCycle(at(1, 1, "again"), ::("s", List("again", "again"))))
  }

  it should "stop as too deep where no key catches a cycle" in {
    var n = 0
    val deeper = directive("deeper") { (_, context) =>
      n += 1
      Right(context.parse("n" + n.toString, "#deeper"))
    }
    errors(loader(deeper), "#deeper") match {
      case List(DirectiveTooDeep(location, limit)) =>
        limit shouldBe Expand.MaxDepth
        location.source shouldBe "n" + (Expand.MaxDepth - 1).toString
      case other => fail(other.toString)
    }
  }

  "The context" should "name the source the call stands in and where" in {
    var seen: Option[(SourceLocation, String, Option[URL])] = None
    val d = directive("d") { (_, context) =>
      seen = Some((context.location, context.source, context.url))
      Right(Fragment.None)
    }
    loaded(loader(d), "a: 1\nb: #d").size shouldBe 1
    seen shouldBe Some((at(2, 4), "s", None))
    val file = write(Files.createTempDirectory("psi"), "a.psi", "#d")
    loader(d)(file.toUri.toURL).isRight shouldBe true
    seen shouldBe
      Some((SourceLocation(file.toUri.toURL.toString, 1, 1), file.toUri.toURL.toString, Some(file.toUri.toURL)))
  }

  it should "parse a source at a URL, hand back one it cannot read, and report one that is not UTF-8" in {
    val dir = Files.createTempDirectory("psi")
    val bytes = dir.resolve("bytes.psi")
    Files.write(bytes, Array[Byte]('a', ':', ' ', -1))
    write(dir, "ok.psi", "x: 1")
    val include = directive("inc", Parameter("file", required = true)) { (arguments, context) =>
      arguments("file") match {
        case Node.IScalar(_, _, file, _) =>
          context.parse(context.url.getOrElse(dir.toUri.toURL).toURI.resolve(file).toURL) match {
            case Right(fragment) => Right(fragment)
            case Left(e: IOException) =>
              if (file.startsWith("optional")) Right(Fragment.None) else Left(Problem(e.getClass.getSimpleName))
          }
        case other => Left(Problem(other.toString))
      }
    }
    val main = write(dir, "main.psi", "#inc(\"ok.psi\")\n#inc(\"optional.psi\")")
    loader(include)(main.toUri.toURL).map(_("x")) shouldBe
      Right(Node.IScalar(keyed("x"), None, "1", SourceLocation(dir.resolve("ok.psi").toUri.toURL.toString, 1, 4)))
    val broken = write(dir, "broken.psi", "#inc(\"missing.psi\")\n#inc(\"bytes.psi\")")
    val url = broken.toUri.toURL.toString
    loader(include)(broken.toUri.toURL).left.map(_.errors) shouldBe
      Left(
        List(
          DirectiveFailed(SourceLocation(url, 1, 1), "inc", Problem("FileNotFoundException")),
          InvalidEncoding(SourceLocation(bytes.toUri.toURL.toString, 1, 4))))
  }

  it should "know a file by its real path, so a link does not hide a cycle" in {
    val dir = Files.createTempDirectory("psi")
    Files.createSymbolicLink(dir.resolve("link"), dir)
    val a = write(dir, "a.psi", "#inc(\"link/b.psi\")")
    write(dir, "b.psi", "#inc(\"a.psi\")")
    val include = directive("inc", Parameter("file", required = true)) { (arguments, context) =>
      arguments("file") match {
        case Node.IScalar(_, _, file, _) =>
          context.parse(context.url.getOrElse(fail("no URL")).toURI.resolve(file).toURL).left.map(e =>
            Problem(e.toString))
        case other => Left(Problem(other.toString))
      }
    }
    loader(include)(a.toUri.toURL).left.map(_.errors) shouldBe
      Left(
        List(
          DirectiveCycle(
            SourceLocation(dir.resolve("link/b.psi").toUri.toURL.toString, 1, 1),
            ::(a.toUri.toURL.toString, List("link/b.psi", "link/a.psi").map(dir.resolve(_).toUri.toURL.toString))
          )))
  }

  "A source's key" should "be its URI normalized, a file's real path, or the URL as written" in {
    Source.key(URI.create("http://h/a/../b.psi").toURL) shouldBe "http://h/b.psi"
    Source.key(URI.create("file:/no/such/x.psi").toURL) shouldBe "file:/no/such/x.psi"
    val file = write(Files.createTempDirectory("psi"), "k.psi", "")
    Source.key(file.toUri.toURL) shouldBe file.toRealPath().toString
    Source("n", "").key shouldBe "n"
  }

  "A DirectiveLocation" should "name the call, the directive and the origin, a name not bare in backticks" in {
    DirectiveLocation(at(1, 4, "app.psi"), "yaml", SourceLocation("db.yaml", 3, 8)).description shouldBe
      "app.psi:1:4 (#yaml from db.yaml:3:8)"
    DirectiveLocation(at(1, 4, "app.psi"), "a.b", NoSourceLocation).description shouldBe
      "app.psi:1:4 (#`a.b` from no source)"
  }

  "A handler's context" should "find a target as #include does, with the handler's extensions" in {
    var found: List[Either[DirectiveError, Option[Target]]] = Nil
    val finding = directive("find", Target.parameters*) { (arguments, context) =>
      found = context.target(arguments, Seq(".txt", ".text")) :: found
      Right(Fragment.None)
    }
    val dir = Files.createTempDirectory("psi")
    write(dir, "b.text", "x")
    val app =
      write(dir, "app.psi", "#find(file: a, optional: true)\n#find(file: a)\n#find(file: b)\n#find(url: x, file: y)")
    loader(finding)(app.toUri.toURL).isRight shouldBe true
    found.reverse shouldBe
      List(
        Right(None),
        Left(IncludeNotFound("a")),
        Right(Some(Target(dir.resolve("b.text").toUri.toURL, "b", optional = false, known = true))),
        Left(IncludeTargets(List("file", "url")))
      )
    found = Nil
    loader(finding).parse("s", "#find(url: \"http://localhost/x\")").isRight shouldBe true
    found shouldBe List(Right(
      Some(Target(URI.create("http://localhost/x").toURL, "http://localhost/x", optional = false, known = false))))
  }

  it should "read a URL's bytes through the load's read limit" in {
    var read: List[Either[IOException, Option[Array[Byte]]]] = Nil
    val reading = directive("read", Parameter("file", required = true)) { (arguments, context) =>
      val url = context.url.get.toURI.resolve(arguments("file") match {
        case Node.IScalar(_, _, file, _) => file
        case other => fail(other.toString)
      }).toURL
      read = context.read(url) :: read
      Right(Fragment.None)
    }
    val dir = Files.createTempDirectory("psi")
    write(dir, "data", "0123456789")
    val app = write(dir, "app.psi", "#read(data)\n#read(missing)")
    loader(reading)(app.toUri.toURL).isRight shouldBe true
    read.reverse match {
      case Right(Some(bytes)) :: Left(_: java.io.FileNotFoundException) :: Nil =>
        new String(bytes, StandardCharsets.UTF_8) shouldBe "0123456789"
      case other => fail(other.toString)
    }
    read = Nil
    val limited = Psi.builder.withDirective(reading._1, reading._2).withReadLimit(30, java.time.Duration.ofSeconds(9))
      .build.fold(e => fail(e.toString), x => x)
    limited(app.toUri.toURL).left.map(_.errors.map(_.getClass.getSimpleName)) shouldBe Left(List("ReadLimitExceeded"))
    read shouldBe List(Right(None), Right(None))
  }

  private def write(dir: Path, name: String, text: String): Path =
    Files.write(dir.resolve(name), text.getBytes(StandardCharsets.UTF_8))
}
