package scala.meta.internal.typeassign

import scala.meta._
import scala.meta.internal.symtab.SymbolTable
import scala.meta.internal.{semanticdb => s}

class TypeAssign(symtab: SymbolTable, occs: Seq[s.SymbolOccurrence]) {

  private def getSymbol(pos: Position) = {
    val Seq(occ) = occs.filter(occ => occ.range.fold(false) { range =>
      pos.startLine == range.startLine &&
      pos.endLine == range.endLine &&
      pos.startColumn == range.startCharacter &&
      pos.endColumn == range.endCharacter
    })
    occ.symbol
  }

  private def signature(term: Term): s.Signature =
    term match {
      case term: Lit.Int => s.ValueSignature(s.TypeRef(symbol = "scala/Int#"))
      case term: Lit.Double => s.ValueSignature(s.TypeRef(symbol = "scala/Double#"))
      case term: Lit.String => s.ValueSignature(s.TypeRef(symbol = "java/lang/String#"))
      case term: Term.Name =>
        val sym = getSymbol(term.pos)
        val sig = symtab.info(getSymbol(term.pos)).get.signature
        sig
      case term: Term.Apply =>
        val s.MethodSignature(_, _, ret) = signature(term.fun)
        s.ValueSignature(ret)
    }

  private def getType(term: Term): s.Type = {
    signature(term) match {
      case s.ValueSignature(tpe) => tpe
      case sig => sys.error(s"expected type when other signature $sig provided")
    }
  }

  def assign(term: Term): s.Type = getType(term)

}
