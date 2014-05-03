import scala.tools.nsc.{Settings}
import scala.tools.nsc.interactive.{Global}
import scala.tools.nsc.interactive.{Response}
import scala.tools.nsc.reporters.ConsoleReporter
import scala.reflect.internal.util.{SourceFile, OffsetPosition}

class ProjectSetting(
  val classPath: Seq[String]
)

object ProjectSetting {
  import scala.io.Source
  import org.ensime.sbt.util.{SExp, SExpList, StringAtom}
  import org.ensime.sbt.KeyMap
  import scala.util.parsing.input.CharSequenceReader

  def fromEnsime(source:Source):Option[ProjectSetting] = {
    import SExp.key
    def keywordMap(sexp:SExp):Option[KeyMap] =  sexp match { case l:SExpList => Some(l.toKeywordMap) case _ => None }
    for {
      ensime:KeyMap <- keywordMap(SExp.read(new CharSequenceReader(source.getLines().filter(!_.startsWith(";;")).mkString("\n"))))
      project:KeyMap <- ensime.get(key(":subprojects")) match { case Some(a) => keywordMap(a) case _ => None }
      compile_deps:Seq[String] <- project.get(key(":compile-deps")) match {
        case Some(l:SExpList) => Some(l.toSeq.flatMap { _ match { case s:StringAtom => Seq(s.value) case _ => Seq() } })
        case _ => None
      }
    } yield {
      new ProjectSetting(
        classPath = Seq()
      )
    }
  }
}


class Project(root: String, setting:ProjectSetting) {
  lazy val nscSettings:Settings = new Settings()
  nscSettings.processArguments(List("-classpath", "/Users/ariyamizutani/.sbt/0.12.4/boot/scala-2.10.4/lib/scala-library.jar:/Users/ariyamizutani/projects/scams/target/scala-2.10/classes:/Users/ariyamizutani/.sbt/0.12.4/boot/scala-2.10.4/lib/scala-compiler.jar:/Users/ariyamizutani/.ivy2/cache/org.scala-lang/scala-reflect/jars/scala-reflect-2.10.4.jar"), true)
  val global:Global = new Global(nscSettings, new ConsoleReporter(nscSettings))
}

object Project {
  import scala.io.Source
  def load(root:String):Option[Project] = {
    for {
      setting <- ProjectSetting.fromEnsime(Source.fromFile(s"$root/.ensime"))
    } yield {
      new Project(root, setting)
    }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val project = Project.load(".") match { case Some(p) => p; case None => throw new RuntimeException("load failed") }
    val sourceFile:SourceFile = project.global.getSourceFile("./src/main/scala/Project.scala")
    val response : Response[project.global.Tree] = new Response()
  }
}
