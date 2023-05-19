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
    PrevWorklistLenght,
    worklistComponent,
    worklistPrevComponent,
    storeLookup,
    storeChanged,
    latticeInteger,
    latticeString,
    latticeBoolean
  ).map(prim => (prim.name, prim)).toMap ++ super.allPrimitives

  case class Storeval(value: stateKeeper.analysis.Value) extends Value:
    override def toString: String = s"#<store-val:$value"
  object WorklistLenght extends SimplePrim:
    val name = "worklist-length"

    def call(args: List[Value], position: Position): Value =
      val state = stateKeeper.currentState
      if state == null
        then
          Value.Integer(0)
      else
        Value.Integer(stateKeeper.currentState.wl.queue.length)

  object PrevWorklistLenght extends SimplePrim:
    val name = "prev-worklist-length"

    def call(args: List[Value], position: Position): Value =
      val state = stateKeeper.lastState
      if state == null
      then
        Value.Integer(0)
      else
        Value.Integer(stateKeeper.lastState.wl.queue.length)


  object worklistComponent extends SimplePrim:
    val name = "worklist-component"

    def call(args: List[Value], position: Position): Value =
      val state = stateKeeper.currentState
      if state != null
      then Value.Str(state.cmp.toString)
      else Value.Str("")

  object worklistPrevComponent extends SimplePrim:
    val name = "worklist-prev-component"

    def call(args: List[Value], position: Position): Value =
      val state = stateKeeper.lastState
      if state != null
      then Value.Str(state.cmp.toString)
      else Value.Str("")

  object storeLookup extends SimplePrim:
    val name = "store-lookup"

    def call(args: List[Value], position: Position): Value =
      //if args.length == 1 then
        val adr = args(1)
        val state = stateKeeper.currentState
        //if state != null then
        Value.Bool(true) //state.sto.content.filter((k,v) => if k.toString == adr then v)

  object storeChanged extends SimplePrim:
    val name = "store-changed?"

    // adres als arg nemen
    def call(args: List[Value], position: Position): Value =
      val state = stateKeeper.currentState
      val prevState = stateKeeper.lastState
      if state != null && prevState != null
        then
        Value.Bool(state.sto == prevState.sto)
        else Value.Bool(true)

  object latticeInteger extends SimplePrim:
    val name = "lattice-integer?"

    def call(args: List[Value], position: Position): Value =
      Value.Bool(true)

  object latticeString extends SimplePrim:
    val name = "lattice-string?"

    def call(args: List[Value], position: Position): Value =
      Value.Bool(true)

  object latticeBoolean extends SimplePrim:
    val name = "lattice-Bool?"

    def call(args: List[Value], position: Position): Value =
      Value.Bool(true)

  // cons, pair? vector? real? char?
