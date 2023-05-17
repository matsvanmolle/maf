package maf.language.scheme.interpreter

import maf.core.Identity
import maf.core.Position.Position
import maf.language.scheme.*
import maf.core.*
import maf.lattice.{MathOps, NumOps}
import maf.modular.scheme.monadic.StateKeeper

trait ConcreteSchemePrimitivesDebugger extends ConcreteSchemePrimitives:
  this: BaseSchemeInterpreter[_] =>

  import NumOps._
  import ConcreteValues._

  val stateKeeper: StateKeeper

  override def allPrimitives: Map[String, Prim] = super.allPrimitives