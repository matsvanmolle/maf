package maf.language.scheme.interpreter

import maf.core.Identity
import maf.core.Position.Position
import maf.language.scheme.*
import maf.core.*
import maf.language.scheme.interpreter.ConcreteValues.Value
import maf.lattice.{MathOps, NumOps}
import maf.modular.scheme.monadic.*
import maf.language.scheme.lattices.SchemeOp

import scala.language.postfixOps

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
            val adr = args.head
            val address: String = adr match
                case Value.Pointer(v) =>getString(v)
                case _ => ""

            val state = stateKeeper.currentState
            if state != null then
                val result: Option[ConcreteSchemePrimitivesDebugger.this.stateKeeper.analysis.Value] = state.sto.content.find { case (key, _) => key.toString == address }.map(_._2)

                result match {
                    case Some(value) => Storeval(value)
                    case None => Value.Bool(false)
                }
            else Value.Bool(false)

    object storeChanged extends SimplePrim:
        val name = "store:changed?"

        // adres als arg nemen
        def call(args: List[Value], position: Position): Value =
            val adr = args.head
            val address: String = adr match
                case Value.Pointer(v) => getString(v)
                case _ => ""

            val state = stateKeeper.currentState
            val prevState = stateKeeper.lastState
            if state != null then
                if prevState != null then
                    val result: Option[ConcreteSchemePrimitivesDebugger.this.stateKeeper.analysis.Value] = state.sto.content.find { case (key, _) => key.toString == address }.map(_._2)
                    val prevResult: Option[ConcreteSchemePrimitivesDebugger.this.stateKeeper.analysis.Value] = prevState.sto.content.find { case (key, _) => key.toString == address }.map(_._2)

                    result match {
                           case Some(value1) =>
                             prevResult match {
                               case Some(value2) => Value.Bool(value1 != value2)
                               case None => Value.Bool(true)
                             }
                           case None => Value.Bool(false)
                     }
                else Value.Bool(false)
            else Value.Bool(false)


    object latticeInteger extends SimplePrim:
        val name = "lattice:Integer?"

        def call(args: List[Value], position: Position): Value =
          val vlu: Option[ConcreteSchemePrimitivesDebugger.this.stateKeeper.analysis.Value] = args(0) match
            case Storeval(v) => Some(v)
            case _ => null
          vlu match
            case Some(vlu) =>
              val res = stateKeeper.analysis.lattice.op(SchemeOp.IsInteger)(List(vlu)) // voor de check uit, resultaat is abtracte boolean
              Value.Bool(stateKeeper.analysis.lattice.isTrue(res.getOrElse(stateKeeper.analysis.lattice.nil))) // kijk of de abstracte boolean true is
            case _ => Value.Bool(false)

    object latticeReal extends SimplePrim:
        val name = "lattice:Real?"

        def call(args: List[Value], position: Position): Value =
          val vlu: Option[ConcreteSchemePrimitivesDebugger.this.stateKeeper.analysis.Value] = args(0) match
            case Storeval(v) => Some(v)
            case _ => null
          vlu match
            case Some(vlu) =>
              val res = stateKeeper.analysis.lattice.op(SchemeOp.IsReal)(List(vlu)) // voor de check uit, resultaat is abtracte boolean
              Value.Bool(stateKeeper.analysis.lattice.isTrue(res.getOrElse(stateKeeper.analysis.lattice.nil))) // kijk of de abstracte boolean true is
            case _ => Value.Bool(false)

    object latticeString extends SimplePrim:
        val name = "lattice:String?"

        def call(args: List[Value], position: Position): Value =
          val vlu: Option[ConcreteSchemePrimitivesDebugger.this.stateKeeper.analysis.Value] = args(0) match
            case Storeval(v) => Some(v)
            case _ => null
          vlu match
            case Some(vlu) =>
              val res = stateKeeper.analysis.lattice.op(SchemeOp.IsString)(List(vlu)) // voor de check uit, resultaat is abtracte boolean
              Value.Bool(stateKeeper.analysis.lattice.isTrue(res.getOrElse(stateKeeper.analysis.lattice.nil))) // kijk of de abstracte boolean true is
            case _ => Value.Bool(false)

    object latticeChar extends SimplePrim:
        val name = "lattice:Char?"

        def call(args: List[Value], position: Position): Value =
          val vlu: Option[ConcreteSchemePrimitivesDebugger.this.stateKeeper.analysis.Value] = args(0) match
            case Storeval(v) => Some(v)
            case _ => null
          vlu match
            case Some(vlu) =>
              val res = stateKeeper.analysis.lattice.op(SchemeOp.IsChar)(List(vlu)) // voor de check uit, resultaat is abtracte boolean
              Value.Bool(stateKeeper.analysis.lattice.isTrue(res.getOrElse(stateKeeper.analysis.lattice.nil))) // kijk of de abstracte boolean true is
            case _ => Value.Bool(false)

    object latticeBoolean extends SimplePrim:
        val name = "lattice:Bool?"

        def call(args: List[Value], position: Position): Value =
          val vlu: Option[ConcreteSchemePrimitivesDebugger.this.stateKeeper.analysis.Value] = args(0) match
            case Storeval(v) => Some(v)
            case _ => null
          vlu match
            case Some(vlu) =>
              val res = stateKeeper.analysis.lattice.op(SchemeOp.IsBoolean)(List(vlu)) // voor de check uit, resultaat is abtracte boolean
              Value.Bool(stateKeeper.analysis.lattice.isTrue(res.getOrElse(stateKeeper.analysis.lattice.nil))) // kijk of de abstracte boolean true is
            case _ => Value.Bool(false)

    object latticeVector extends SimplePrim:
        val name = "lattice:Vector?"

        def call(args: List[Value], position: Position): Value =
          val vlu: Option[ConcreteSchemePrimitivesDebugger.this.stateKeeper.analysis.Value] = args(0) match
            case Storeval(v) => Some(v)
            case _ => null
          vlu match
            case Some(vlu) =>
              val res = stateKeeper.analysis.lattice.op(SchemeOp.IsVector)(List(vlu)) // voor de check uit, resultaat is abtracte boolean
              Value.Bool(stateKeeper.analysis.lattice.isTrue(res.getOrElse(stateKeeper.analysis.lattice.nil))) // kijk of de abstracte boolean true is
            case _ => Value.Bool(false)

    object latticePair extends SimplePrim:
        val name = "lattice:Pair?"

        def call(args: List[Value], position: Position): Value =
          val vlu: Option[ConcreteSchemePrimitivesDebugger.this.stateKeeper.analysis.Value] = args(0) match
            case Storeval(v) => Some(v)
            case _ => null
          vlu match
            case Some(vlu) =>
              val res = stateKeeper.analysis.lattice.op(SchemeOp.IsCons)(List(vlu)) // voor de check uit, resultaat is abtracte boolean
              Value.Bool(stateKeeper.analysis.lattice.isTrue(res.getOrElse(stateKeeper.analysis.lattice.nil))) // kijk of de abstracte boolean true is
            case _ => Value.Bool(false)

    object latticeCar extends SimplePrim:
        val name = "lattice:car"

        def call(args: List[Value], position: Position): Value =
          Value.Bool(true)

    object latticeCdr extends SimplePrim:
        val name = "lattice:cdr"

        def call(args: List[Value], position: Position): Value =
            Value.Bool(true)
