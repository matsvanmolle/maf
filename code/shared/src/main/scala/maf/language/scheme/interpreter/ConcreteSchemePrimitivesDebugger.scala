package maf.language.scheme.interpreter

import maf.core.Identity
import maf.core.Position.Position
import maf.language.scheme.*
import maf.core.*
import maf.lattice.{MathOps, NumOps}
import maf.modular.scheme.monadic.*

trait ConcreteSchemePrimitivesDebugger[A <: SimpleModFAnalysis] extends ConcreteSchemePrimitives:
    this: BaseSchemeInterpreter[_] =>

    import NumOps._
    import ConcreteValues._

    val stateKeeper: StateKeeper[A]

    override def allPrimitives: Map[String, Prim] = List(
      WorklistLenght,
      PrevWorklistLenght,
      worklistComponent,
      worklistPrevComponent,
      storeLookup,
      storeChanged,
      latticeInteger,
      latticeReal, 
      latticeBoolean,
      latticeString,
      latticeChar,
      latticeVector,
      latticePair,
      latticeCar,
      latticeCdr
    ).map(prim => (prim.name, prim)).toMap ++ super.allPrimitives

    case class Storeval(value: stateKeeper.analysis.Value) extends Value:
        override def toString: String = s"#<store-val:$value"
    object WorklistLenght extends SimplePrim:
        val name = "wl:length"

        def call(args: List[Value], position: Position): Value =
            val state = stateKeeper.currentState
            if state == null then 
                println("--------wl len no state---------")
                Value.Integer(0)
            else 
                println("-------wl len----------")
                println(stateKeeper.currentState.wl.queue.length)
                Value.Integer(stateKeeper.currentState.wl.queue.length)

    object PrevWorklistLenght extends SimplePrim:
        val name = "w:prev-length"

        def call(args: List[Value], position: Position): Value =
            val state = stateKeeper.lastState
            if state == null then Value.Integer(0)
            else Value.Integer(stateKeeper.lastState.wl.queue.length)

    object worklistComponent extends SimplePrim:
        val name = "wl:component"

        def call(args: List[Value], position: Position): Value =
            val state = stateKeeper.currentState
            if state != null then Value.Str(state.cmp.toString)
            else Value.Str("")

    object worklistPrevComponent extends SimplePrim:
        val name = "wl:prev-component"

        def call(args: List[Value], position: Position): Value =
            val state = stateKeeper.lastState
            if state != null then Value.Str(state.cmp.toString)
            else Value.Str("")

    object storeLookup extends SimplePrim:
        val name = "store:lookup"

        def call(args: List[Value], position: Position): Value =
            //if args.length == 1 then
            val adr = args(1)
            val state = stateKeeper.currentState
            //if state != null then
            Value.Bool(true) //state.sto.content.filter((k,v) => if k.toString == adr then v)

    object storeChanged extends SimplePrim:
        val name = "store:changed?"

        // adres als arg nemen
        def call(args: List[Value], position: Position): Value =
            val state = stateKeeper.currentState
            val prevState = stateKeeper.lastState
            if state != null && prevState != null then Value.Bool(state.sto == prevState.sto)
            else Value.Bool(true)
            

    object latticeInteger extends SimplePrim:
        val name = "lattice:Integer?"

        def call(args: List[Value], position: Position): Value =
            Value.Bool(true)

    object latticeReal extends SimplePrim:
        val name = "lattice:Real?"

        def call(args: List[Value], position: Position): Value =
            Value.Bool(true)        

    object latticeString extends SimplePrim:
        val name = "lattice:String?"

        def call(args: List[Value], position: Position): Value =
            Value.Bool(true)

    object latticeChar extends SimplePrim:
        val name = "lattice:Char?"

        def call(args: List[Value], position: Position): Value =
            Value.Bool(true)        

    object latticeBoolean extends SimplePrim:
        val name = "lattice:Bool?"

        def call(args: List[Value], position: Position): Value =
            Value.Bool(true)

    object latticeVector extends SimplePrim:
        val name = "lattice:Vector?"

        def call(args: List[Value], position: Position): Value =
            Value.Bool(true)

    object latticePair extends SimplePrim:
        val name = "lattice:Pair?"

        def call(args: List[Value], position: Position): Value =
            Value.Bool(true)

    object latticeCar extends SimplePrim:
        val name = "lattice:car"

        def call(args: List[Value], position: Position): Value =
            Value.Bool(true)

    object latticeCdr extends SimplePrim:
        val name = "lattice:cdr"

        def call(args: List[Value], position: Position): Value =
            Value.Bool(true)




// cons, pair? vector? real? char?
