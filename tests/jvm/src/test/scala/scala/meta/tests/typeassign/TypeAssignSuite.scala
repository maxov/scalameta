package scala.meta.tests.typeassign

import java.nio.file.Paths
import org.scalatest.FunSuite
import scala.meta.internal.io.FileIO
import scala.meta.io.AbsolutePath
import scala.meta.tests.BuildInfo
import scala.meta._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.symtab.{GlobalSymbolTable, LocalSymbolTable}
import scala.meta.internal.typeassign.TypeAssign

class TypeAssignSuite extends FunSuite {

  private def collectCheckAssigns(tree: Tree): Seq[(String, Defn.Val)] = tree match {
    case tree: Source => tree.stats.flatMap(collectCheckAssigns)
    case tree: Pkg => tree.stats.flatMap(collectCheckAssigns)
    case tree: Defn.Object => tree.templ.stats.flatMap(collectCheckAssigns)
    case tree: Defn.Val if tree.pats.length == 1 =>
      tree.pats match {
        case List(Pat.Var(Term.Name(name))) if name.startsWith("check_") => Seq((name, tree))
        case _ => Seq()
      }
    case _ =>
      Seq()
  }

  test("Type assign integration") {
    val out = Paths.get(BuildInfo.integrationTypeassignClasspath)

    val basicSemanticdb = {
      val result = out.resolve("META-INF/semanticdb/semanticdb/integration-typeassign/src/main/scala/example/BasicTest.scala.semanticdb")
      val text = s.TextDocuments.parseFrom(FileIO.readAllBytes(AbsolutePath(result)))
      text.documents.head
    }

    val checkAssignSymbols = basicSemanticdb.symbols.collect {
      case info if info.symbol.desc.name.startsWith("check_") => info.symbol.desc.name -> info
    }.toMap

    val parsed = Input.File(Paths.get(BuildInfo.integrationTypeassignSourcepath).resolve("example/BasicTest.scala"))
    val checkAssigns = collectCheckAssigns(parsed.parse[Source].get)
    val checkAssignsWithInfo = checkAssigns.map {
      case (name, tree) => (name, tree, checkAssignSymbols(name))
    }
    val symtab = LocalSymbolTable(basicSemanticdb.symbols)
    println(basicSemanticdb.synthetics)
    val typeAssign = new TypeAssign(symtab, basicSemanticdb.occurrences)
    checkAssignsWithInfo foreach {
      case (_, tree, info) =>
        val s.MethodSignature(_, _, ret) = info.signature
        val typeAssignRet = typeAssign.assign(tree.rhs)
        assert(typeAssignRet == ret, s"for term: ${tree.rhs}")
    }
  }

}
