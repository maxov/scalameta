package scala.meta.metanl

import java.nio.file.{Path, Paths}
import scala.meta.cli.Reporter
import scala.meta.io.Classpath

case class Settings(
  classpath: Option[Path] = None,
  outRoot: Option[Path] = None
)

object Settings {
  def parse(args: List[String], reporter: Reporter): Option[Settings] = {
    def loop(settings: Settings, allowOptions: Boolean, args: List[String]): Option[Settings] = {
      args match {
        case "--" +: rest =>
          loop(settings, false, rest)
        case "-o" +: outPath +: rest =>
          loop(settings.copy(outRoot = Some(Paths.get(outPath))), true, rest)
        case flag +: _ if allowOptions && flag.startsWith("-") =>
          reporter.err.println(s"unsupported flag $flag")
          None
        case cp +: rest =>
          loop(settings.copy(classpath = Some(Paths.get(cp))), true, rest)
        case Nil =>
          Some(settings)
      }
    }
    loop(Settings(), allowOptions = true, args).filter { settings =>
      if (settings.classpath.isEmpty) reporter.err.println("classpath not given")
      if (settings.outRoot.isEmpty) reporter.err.println("outRoot not given")
      settings.classpath.isDefined && settings.outRoot.isDefined
    }
  }
}
