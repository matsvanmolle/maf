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

  override def allPrimitives: Map[String, Prim] = List(
    WorklistLenght, 
    PrevWorklistLenght
  ).map(prim => (prim.name, prim)).toMap ++ super.allPrimitives

  object WorklistLenght extends SimplePrim:
    val name = "workList-length"
    def call(args: List[Value], position: Position): Value =
      //val state = stateKeeper.currentState
      Value.Integer(0)
      /*if state == null
        then 
          Value.Integer(0)
      else
        Value.Integer(stateKeeper.currentState.wl.queue.length)*/

  object PrevWorklistLenght extends SimplePrim:
    val name = "prevWorkList-length"

    def call(args: List[Value], position: Position): Value =
      val state = stateKeeper.lastState
      if state == null
        then 
        Value.Integer(0)
      else 
        Value.Integer(stateKeeper.lastState.wl.queue.length)
        
        
      
  
  