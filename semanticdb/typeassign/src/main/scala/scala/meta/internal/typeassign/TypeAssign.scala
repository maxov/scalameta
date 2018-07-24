package scala.meta.internal.typeassign

import scala.meta._
import scala.meta.internal.symtab.SymbolTable
import scala.meta.internal.{semanticdb => s}

class TypeAssign(
    source: Source,
    symtab: SymbolTable,
    occs: Seq[s.SymbolOccurrence],
    synths: Seq[s.Synthetic]) {

  private def getSymbol(range: s.Range) = {
    val Seq(occ) = occs.filter(occ => occ.range.contains(range))
    occ.symbol
  }

  private def getTree(range: s.Range): Tree = {
    def collectTreesAt(tree: Tree): List[Tree] = {
      val restTrees = tree.children.flatMap(collectTreesAt)
      if (tree.pos.toRange == range) tree +: restTrees else restTrees
    }
    val List(only) = collectTreesAt(source)
    only
  }

  private def getTerm(range: s.Range): Term = getTree(range).asInstanceOf[Term]

  private def getSynthetic(range: s.Range): Option[s.Synthetic] = {
    synths.filter(synth => synth.range.contains(range)) match {
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

  private def signatureRange(range: s.Range, tree: Option[Term] = None): s.Signature =
    getSynthetic(range) match {
      case Some(synth) => signature(synth.tree)
      case None => signature(tree.getOrElse(getTerm(range)))
    }

  private def signature(tree: s.Tree): s.Signature = tree match {
    case s.OriginalTree(Some(range)) => signature(getTerm(range))
    case s.TypeApplyTree(fn, targs) =>
      val s.MethodSignature(tparams, _, ret) = signature(fn)
      val finalType = Substitution(tparams, targs)(ret)
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
        val s.MethodSignature(_, _, ret) = signatureRange(term.fun.pos.toRange, Some(term.fun))
        s.ValueSignature(ret)
      case term: Term.ApplyType =>
        val s.MethodSignature(tparams, _, ret) = signature(term.fun)
        val targs: Seq[s.Type] = term.targs.map(toSemanticdbType)
        val finalType = Substitution(tparams, targs)(ret)
        s.MethodSignature(returnType = finalType)
      case term: Term.Select =>
        val qualType = asTermSignature(signature(term.qual)).get.tpe
        val nameSig = signature(term.name)
        Substitution(qualType)(nameSig)
      case _ => sys.error(s"unsupported tree: $term")
    }

  private def asTermSignature(sig: s.Signature): Option[s.ValueSignature] = sig match {
    case sig: s.ValueSignature => Some(sig)
    case s.MethodSignature(tParams, parameterLists, returnType) =>
      // 0-argument method in position of a term is the return type of the method
      parameterLists match {
        case Seq() | Seq(s.Scope(Seq(), Seq())) => Some(s.ValueSignature(returnType))
        case _ => None
      }
    case _ => None
  }

  def assign(term: Term): s.Type = {
    val sig = signature(term)
    asTermSignature(sig).get.tpe
  }

  implicit class PositionOps(pos: Position) {
    def toRange: s.Range = s.Range(
      startLine = pos.startLine,
      endLine = pos.endLine,
      startCharacter = pos.startColumn,
      endCharacter = pos.endColumn
    )
  }

  /**
    * Models a substitution from a list of symbols representing type variables, to the types
    * those symbols should become. The apply methods perform the substitution.
    */
  private class Substitution(mapping: Map[String, s.Type]) {

    def apply(tpe: s.Type): s.Type = tpe match {
      case tpe @ s.TypeRef(_, sym, _) =>
        mapping.getOrElse(sym, tpe.copy(typeArguments = tpe.typeArguments.map(apply)))
      case _ => sys.error(s"not implemented $tpe")
    }

    def apply(sig: s.Signature): s.Signature = sig match {
      case s.ValueSignature(tpe) => s.ValueSignature(apply(tpe))
      case sig: s.MethodSignature =>
        sig.copy(returnType = apply(sig.returnType))
      case _ => sys.error(s"cannot substitute on signature $sig")
    }

  }

  private object Substitution {

    def apply(mapping: Map[String, s.Type]): Substitution = new Substitution(mapping)

    def apply(tparams: Option[s.Scope], targs: Seq[s.Type]): Substitution = {
      val symlinks = tparams.get.symlinks
      assert(symlinks.nonEmpty)
      Substitution(symlinks.zip(targs).toMap)
    }

    /**
      * Build a substitution based on some applied type
      */
    def apply(qualType: s.Type): Substitution = qualType match {
      case s.TypeRef(_, sym, Seq()) => Substitution(Map.empty[String, s.Type])
      case s.TypeRef(_, sym, targs) =>
        val symSig = symtab
          .info(sym)
          .map(_.signature)
          .collect {
            case sig: s.ClassSignature => sig
          }
          .get
        val tparams = symSig.typeParameters
        Substitution(tparams, targs)
    }

  }

}
