package scams


case class ProjectSetting(
  val classPath: Seq[String],
  val sourceRoots: Seq[String]
)

object ProjectSetting {
  import scala.io.Source

  def fromEnsime(source:Source):Either[String, ProjectSetting] = {
    import org.ensime.sbt.KeyMap
    import org.ensime.sbt.util.{SExp, SExpList, StringAtom}
    import scala.util.parsing.input.CharSequenceReader
    import scala.tools.nsc.interactive.{Global}
    import scala.reflect.internal.util.{SourceFile, OffsetPosition}
    import SExp.key

    def keywordMap(sexp:SExp):Either[String, KeyMap] =  sexp match { case l:SExpList => Right(l.toKeywordMap) case s => Left(s"Not List: ${s.toReadableString}") }
    def toStringSeq(sexp:Option[SExp]):Either[String, Seq[String]] = sexp match {
      case Some(l:SExpList) => Right(l.toSeq.flatMap { _ match { case s:StringAtom => Seq(s.value) case _ => Seq() } })
      case _ => Left("invalid :compile-deps")
    }
    for {
      ensime <- keywordMap(SExp.read(new CharSequenceReader(source.getLines().filter(!_.startsWith(";;")).mkString("\n")))).right
      project <- (ensime.get(key(":subprojects")) match {
        case Some(l:SExpList) => l.headOption match {
          case Some(a) => keywordMap(a)
          case None => Left(":subprojects is empty")
        }
        case _ => Left(":subprojects error")
      }).right
      compileDeps <- toStringSeq(project.get(key(":compile-deps"))).right
      sourceRoots <- toStringSeq(project.get(key(":source-roots"))).right
    } yield {
      new ProjectSetting(
        classPath = compileDeps,
        sourceRoots = sourceRoots
      )
    }
  }
}


case class Project(root: String, setting:ProjectSetting) {
  import scala.tools.nsc.interactive.{Global}
  import scala.tools.nsc.{Settings}
  import scala.tools.nsc.reporters.ConsoleReporter

  val nscSettings:Settings = new Settings()
  nscSettings.processArguments(List("-classpath", setting.classPath.mkString(":")), true)

  val global:Global = new Global(nscSettings, new ConsoleReporter(nscSettings))

  def shutdown():Unit = global.askShutdown()
}

object Project {
  import scala.io.Source
  def load(root:String):Either[String, Project] = {
    for {
      setting <- ProjectSetting.fromEnsime(Source.fromFile(s"$root/.ensime")).right
    } yield {
      new Project(root, setting)
    }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val project = Project.load(".") match { case Right(p) => p; case Left(msg) => throw new RuntimeException(s"load failed: $msg") }

    println(s"loaded: ${project}")

    processCommand(project)

    project.shutdown()
  }

  def processCommand(project:Project):Unit = {
    print(">> ")
    val line = readLine
    if(line == null || line == "exit") {
      return
    }

    line.split("""\s+""") match {
      case Array() =>
      case Array("type", name) => {
        println(project.global.ask { () => project.global.rootMirror.staticClass(name) })
      }
      case _ => println("What?")
    }

    processCommand(project)
  }
}
