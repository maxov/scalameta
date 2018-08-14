package scala.meta.internal.metanl

import java.io.FileOutputStream
import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.meta.cli.Reporter
import scala.meta.metanl.Settings
import scala.meta.internal.io._
import scala.meta.internal.semanticdb.Locator
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala._

class Main(settings: Settings, reporter: Reporter) {

  private class SymbolVisitor(
      symsMap: Map[String, s.SymbolInformation],
      declPositions: Map[String, s.Range]) {
    val symbolOrder = mutable.ListBuffer[String]()

    def visit(scope: s.Scope): Unit = {
      scope.hardlinks.foreach(info => visit(info))
      scope.symlinks.foreach { sym =>
        if (!symsMap.contains(sym)) {
          println("did not visit:" + sym)
          symbolOrder += sym
        } else visit(symsMap(sym))
      }
    }

    def visit(info: s.SymbolInformation, visitingPosition: Boolean = false): Unit = {
      if (visitingPosition || (!symbolOrder.contains(info.symbol) && !declPositions.contains(
            info.symbol))) {
        symbolOrder += info.symbol
        info.kind match {
          case s.SymbolInformation.Kind.METHOD | s.SymbolInformation.Kind.CONSTRUCTOR =>
            val sig = info.signature.asInstanceOf[s.MethodSignature]
            sig.typeParameters.foreach(visit)
            sig.parameterLists.foreach(visit)
          case s.SymbolInformation.Kind.CLASS =>
            val sig = info.signature.asInstanceOf[s.ClassSignature]
            sig.declarations.foreach(visit)
            sig.typeParameters.foreach(visit)
          case _ =>
        }
      }
    }
  }

  private def transform(doc: s.TextDocument): s.TextDocument = {

    val symsMap = doc.symbols.map { info =>
      info.symbol -> info
    }.toMap

    val declPositions = doc.occurrences
      .filter(_.role == s.SymbolOccurrence.Role.DEFINITION)
      .map { occ =>
        occ.symbol -> occ.range.get
      }
      .toMap

    println(doc.uri)

//    println(doc.toProtoString)

    val infosWithPositions = doc.symbols
      .collect {
        case info if declPositions.contains(info.symbol) => info -> declPositions(info.symbol)
      }
      .sortBy(_._2)

    val visitor = new SymbolVisitor(symsMap, declPositions)

    for ((info, _) <- infosWithPositions) {
      visitor.visit(info, visitingPosition = true)
    }

    for {
      sym <- doc.symbols.map(_.symbol)
    } {
      if (!visitor.symbolOrder.contains(sym)) {
        print("*** NOT COVERED ")
      }
      println(sym, symsMap(sym), declPositions.get(sym))
    }
    doc
  }

  def process(): Boolean = {
    val cp = settings.classpath.get
    val outRoot = settings.outRoot.get
    Locator(cp) { (path, payload) =>
      val outPath = outRoot.resolve(cp.relativize(path))
      val newPayload = s.TextDocuments(payload.documents.map(transform))
      Files.createDirectories(outPath.getParent)
      val os = Files.newOutputStream(outPath)
      try newPayload.writeTo(os)
      finally os.close()
    }
    true
  }

  implicit val rangeOrdering: Ordering[s.Range] =
    Ordering.by(r => (r.startLine, r.startCharacter, r.endLine, r.endCharacter))

}
