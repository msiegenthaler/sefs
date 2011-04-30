package sefs.effect.plugin

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.ast.TreeBrowsers
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import nsc.symtab.Flags

class IoPreserver(val global: Global) extends Plugin {
  import global._

  val name = "iopreserver"
  val description = "Finds 'lost' IO and AIO values (not assigned to anything)"
  val components = List[PluginComponent](Component)

  lazy val IO = definitions.getClass("sefs.effect.IO")
  lazy val UnitT = definitions.getClass("scala.Unit")

  private object Component extends PluginComponent {
    val global: IoPreserver.this.global.type = IoPreserver.this.global
    val runsAfter = List("refchecks")
    val phaseName = IoPreserver.this.name
    def newPhase(_prev: Phase) = new PreserveIOsPhase(_prev)

    class PreserveIOsPhase(prev: Phase) extends StdPhase(prev) {
      override def name = IoPreserver.this.name

      protected def isIoType(tpe: Symbol) = tpe == IO

      def apply(unit: CompilationUnit) {
        //global.treeBrowsers.create().browse(unit.body)
        checkElement(unit, unit.body, UnitT)
      }

      private def checkElement(unit: CompilationUnit, e: Tree, preservedType: Symbol): Unit = e match {
        case a: Apply =>
          val t = a.tpe.resultType.typeSymbol
          if (isIoType(t) && !isIoType(preservedType))
            unit.warning(a.pos, "IO operation is thrown away unexecuted")
        case s: Select =>
          val t = s.tpe.resultType.typeSymbol
          if (isIoType(t) && !isIoType(preservedType))
            unit.warning(s.pos, "IO operation is thrown away unexecuted")
        case d: DefDef =>
          checkTree(unit, d, d.tpt)
        case v: ValDef =>
          checkTree(unit, v, v.tpt)
        case b: Block =>
          checkTree(unit, b, b)
        case other =>
          other.children.foreach(e => checkElement(unit, e, UnitT))
      }
      private def checkTree(unit: CompilationUnit, tree: Tree, t: Tree) = {
        val (s, r) = seperateLast(tree.children)
        s.foreach(e => checkElement(unit, e, UnitT))
        r.foreach(e => checkElement(unit, e, t.tpe.resultType.typeSymbol))
      }
      private def seperateLast[A](v: Traversable[A]): (Traversable[A], Traversable[A]) = if (v.isEmpty) (Nil, Nil) else v.splitAt(v.size - 1)
   }
  }
}