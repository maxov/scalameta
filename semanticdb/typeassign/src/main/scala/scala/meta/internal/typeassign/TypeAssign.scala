package scala.meta.internal.typeassign

import scala.meta._
import scala.meta.internal.semanticdb.TreeMessage.SealedValue.OriginalTree
import scala.meta.internal.symtab.SymbolTable
import scala.meta.internal.{semanticdb => s}

class TypeAssign(source: Source, symtab: SymbolTable, occs: Seq[s.SymbolOccurrence], synths: Seq[s.Synthetic]) {

  private def getSymbol(range: s.Range) = {
    val Seq(occ) = occs.filter(occ => occ.range.fold(false)(_ == range))
    occ.symbol
  }

  private def getTree(range: s.Range) = {
    def collectTreesAt(tree: Tree): List[Tree] = {
      val restTrees = tree.children.flatMap(collectTreesAt)
      if (tree.pos.toRange == range) tree +: restTrees else restTrees
    }
    val List(only) = collectTreesAt(source)
    only
  }

  private def getSynthetic(range: s.Range): Option[s.Synthetic] = {
    synths.filter(synth => synth.range.fold(false)(_ == range)) match {
      case Seq(synth) => Some(synth)
      case _ => None
    }
  }

  private def toSemanticdbType(tpe: Type): s.Type = {
    tpe match {
      case tpe: Type.Name =>
        val sym = getSymbol(tpe.pos.toRange)
        s.TypeRef(symbol = sym)
    }
  }

  private def mkSubstitutions(tparams: Option[s.Scope], args: Seq[s.Type]): Map[String, s.Type] = {
    val symlinks = tparams.get.symlinks
    assert(symlinks.nonEmpty)
    symlinks.zip(args).toMap
  }

  private def substitute(substitutions: Map[String, s.Type])(tpe: s.Type): s.Type = {
    val substituter = substitute(substitutions) _
    tpe match {
      case tpe @ s.TypeRef(_, sym, _) =>
        substitutions.getOrElse(sym, tpe.copy(typeArguments = tpe.typeArguments.map(substituter)))
      case _ => sys.error(s"not implemented $tpe")
    }
  }

  private def signature(tree: s.Tree): s.Signature = tree match {
    case s.OriginalTree(Some(range)) => signature(getTree(range).asInstanceOf[Term])
    case s.TypeApplyTree(fn, targs) =>
      val s.MethodSignature(tparams, _, ret) = signature(fn)
      val finalType = substitute(mkSubstitutions(tparams, targs))(ret)
      s.MethodSignature(returnType = finalType)
    case _ => sys.error(s"unsupported synthetic tree: $tree")
  }

  private def signature(term: Term): s.Signature =
    term match {
      case term: Lit.Int => s.ValueSignature(s.TypeRef(symbol = "scala/Int#"))
      case term: Lit.Double => s.ValueSignature(s.TypeRef(symbol = "scala/Double#"))
      case term: Lit.String => s.ValueSignature(s.TypeRef(symbol = "java/lang/String#"))
      case term: Term.Name =>
        val sym = getSymbol(term.pos.toRange)
        val sig = symtab.info(sym).get.signature
        sig
      case term: Term.Apply =>
        val s.MethodSignature(_, _, ret) = getSynthetic(term.fun.pos.toRange).fold(signature(term.fun)) { synth =>
          signature(synth.tree)
        }
        s.ValueSignature(ret)
      case term: Term.ApplyType =>
        val s.MethodSignature(tparams, _, ret) = signature(term.fun)
        val targs: Seq[s.Type] = term.targs.map(toSemanticdbType)
        val finalType = substitute(mkSubstitutions(tparams, targs))(ret)
        s.MethodSignature(returnType = finalType)
      case _ => sys.error(s"unsupported tree: $term")
    }

  private def getType(term: Term): s.Type = {
    signature(term) match {
      case s.ValueSignature(tpe) => tpe
      case s.MethodSignature(tParams, parameterLists, returnType) =>
        parameterLists match {
          case Seq() | Seq(s.Scope(Seq(), Seq())) => returnType
        }
      case sig => sys.error(s"expected type when other signature $sig provided")
    }
  }

  def assign(term: Term): s.Type = {
    getType(term)
  }

  implicit class PositionOps(pos: Position) {
    def toRange: s.Range = s.Range(
      startLine = pos.startLine,
      endLine = pos.endLine,
      startCharacter = pos.startColumn,
      endCharacter = pos.endColumn
    )
  }

}
