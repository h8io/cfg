package h8io.cfg.impl.psi

import java.time.Duration

/** What a [[PsiLoader]] is built with (`format.md` §5, §6, §11), every field validated by [[PsiBuilder.build]].
  *
  * @param properties
  *   the system properties `#prop` reads, `sys.props` at every load when none
  * @param environment
  *   the environment variables `#env` reads, `sys.env` when none
  */
private[psi] final case class Settings(
    directives: Vector[Directive] = Vector.empty,
    disabled: Vector[String] = Vector.empty,
    markers: Vector[Marker] = Vector.empty,
    properties: Option[Map[String, String]] = None,
    environment: Option[Map[String, String]] = None,
    depthLimit: Int = PsiLoader.DefaultDepthLimit,
    nodeLimit: Int = PsiLoader.DefaultNodeLimit,
    characterLimit: Int = PsiLoader.DefaultCharacterLimit,
    readBytes: Int = ReadBudget.DefaultBytes,
    readTime: Duration = ReadBudget.DefaultTime)
