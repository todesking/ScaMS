package scams

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

  def fromEnsime(source:Source):Either[String, ProjectSetting] = {
    import SExp.key
    def keywordMap(sexp:SExp):Either[String, KeyMap] =  sexp match { case l:SExpList => Right(l.toKeywordMap) case s => Left(s"Not List: ${s.toReadableString}") }
    for {
      ensime <- keywordMap(SExp.read(new CharSequenceReader(source.getLines().filter(!_.startsWith(";;")).mkString("\n")))).right
      project <- (ensime.get(key(":subprojects")) match {
        case Some(l:SExpList) => l.headOption match {
          case Some(a) => keywordMap(a)
          case None => Left(":subprojects is empty")
        }
        case _ => Left(":subprojects error")
      }).right
      compileDeps <- (project.get(key(":compile-deps")) match {
        case Some(l:SExpList) => Right(l.toSeq.flatMap { _ match { case s:StringAtom => Seq(s.value) case _ => Seq() } })
        case _ => Left("invalid :compile-deps")
      }).right
    } yield {
      new ProjectSetting(classPath = compileDeps)
    }
  }
}


class Project(root: String, setting:ProjectSetting) {
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
        println(project.global.mirrorThatLoaded(null).staticClass(name))
      }
      case _ => println("What?")
    }

    processCommand(project)
  }
}
