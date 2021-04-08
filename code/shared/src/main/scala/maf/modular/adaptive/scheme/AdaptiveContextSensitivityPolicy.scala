package maf.modular.adaptive.scheme

import maf.language.scheme._
import maf.modular.adaptive.scheme._
import maf.core.Position._
import maf.modular.scheme.modf.SchemeModFComponent._
import maf.util.Monoid

trait AdaptiveContextSensitivityPolicy extends AdaptiveContextSensitivity {

  // context-sensitivity policy can be configured per closure
  // choice of policies is left open as a parameter; the following needs to be provided:
  // - a starting policy `defaultPolicy` (with high precision)
  // - a `nextPolicy` method, which computes for a given closure the next policy (with lower precision)

  trait ContextSensitivityPolicy {
    def allocCtx(
        clo: lattice.Closure,
        args: List[Value],
        call: Position,
        caller: Component
      ): ComponentContext
    def adaptCtx(ctx: ComponentContext): ComponentContext
  }

  val defaultPolicy: ContextSensitivityPolicy
  def nextPolicy(
      fun: SchemeLambdaExp,
      cur: ContextSensitivityPolicy,
      cts: Set[ComponentContext]
    ): ContextSensitivityPolicy
}

//
// Adaptive KCFA
//

trait AdaptiveKCFA extends AdaptiveContextSensitivityPolicy {

  type ComponentContext = List[Position]

  case object KUnlimited extends ContextSensitivityPolicy {
    override def toString = "k = ∞"
    def adaptCtx(ctx: ComponentContext): ComponentContext = ctx
    def allocCtx(
        clo: lattice.Closure,
        args: List[Value],
        call: Position,
        caller: Component
      ): ComponentContext = call :: getContext(caller)
  }

  case class KCallSites(k: Int) extends ContextSensitivityPolicy {
    override def toString = s"k = $k"
    def adaptCtx(ctx: ComponentContext): ComponentContext = ctx.take(k)
    def allocCtx(
        clo: lattice.Closure,
        args: List[Value],
        call: Position,
        caller: Component
      ): ComponentContext = (call :: getContext(caller)).take(k)
  }

  val defaultPolicy = KUnlimited
  def nextPolicy(
      fun: SchemeLambdaExp,
      cur: ContextSensitivityPolicy,
      cts: Set[ComponentContext]
    ): ContextSensitivityPolicy = cur match {
    case KUnlimited =>
      val highestK = cts.maxBy(_.length).length
      KCallSites(highestK - 1)
    case KCallSites(k) if k > 0 => KCallSites(k - 1)
    case _                      => throw new Exception("Can not lower precision any further!")
  }

  def updateCtx(update: Component => Component)(ctx: ComponentContext) = ctx

  private def getContext(cmp: Component): ComponentContext = view(cmp) match {
    case Main                                   => List.empty
    case cll: Call[ComponentContext] @unchecked => cll.ctx
  }
}
