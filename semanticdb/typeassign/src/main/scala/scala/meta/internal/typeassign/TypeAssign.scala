package scala.meta.internal.typeassign

import scala.meta._
import scala.meta.internal.symtab.SymbolTable
import scala.meta.internal.{semanticdb => s}

class TypeAssign(symtab: SymbolTable, occs: Seq[s.SymbolOccurrence]) {

  def assign(term: Term): s.Type =
    term match {
      case term: Lit.Int => s.TypeRef(symbol = "scala/Int#")
      case term: Lit.Double => s.TypeRef(symbol = "scala/Double#")
      case term: Lit.String => s.TypeRef(symbol = "java/lang/String#")
    }

}
