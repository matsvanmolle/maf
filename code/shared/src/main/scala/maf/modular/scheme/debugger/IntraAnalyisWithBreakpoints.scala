package maf.modular.scheme.debugger

import maf.modular.scheme.modflocal.SchemeSemantics
import maf.language.scheme.SchemeExp
import maf.modular.scheme.SchemeDomain
import maf.modular.scheme.modflocal.SchemeModFLocalSensitivity
import maf.language.scheme.*
import maf.core.Identifier
import maf.modular.scheme.monadic.*
import maf.core.Monad.MonadSyntaxOps
import maf.language.scheme.interpreter.*
import maf.modular.scheme.monadic.*

import scala.concurrent.duration.*
import maf.util.benchmarks.Timeout

//import java.util.concurrent.TimeUnit

import scala.io.StdIn
import scala.annotation.tailrec

trait IntraAnalyisWithBreakpoints extends Monolith:
    this: SchemeDomain with SchemeModFLocalSensitivity with SimpleModFAnalysis =>

    var contin = () => println("noting to do!")
    var isStep: Boolean = false;
    var stateKeeper: StateKeeper[this.type] = _

    import analysisM_.*

    type A[X] = SuspendM[X]

    import maf.language.scheme
    def loop(step: Boolean): Unit
    override def eval(exp: SchemeExp): A[Val] =
        exp match
            case DebuggerBreak(pred, idn) =>
                for
                    state <- get
                    _ = stateKeeper.breakLineNumber = idn.pos.line
                    _ = stateKeeper.newState(state)
                    _ = println("---try to update state----")
                    evaledPred = SchemeInterpreterDebugger.evalPredicate(pred, stateKeeper)
                    _ = println(evaledPred)
                    res <- if evaledPred then breakAndPrint() else unit(lattice.nil)
                yield res


            case _ =>
                if isStep then
                    isStep = false
                    stepAndPrint(exp)
                else super.eval(exp)

    def breakAndPrint(): A[Val] =
        println("mynicebreak")
        for
            _ <- suspend(())

            result <- unit(lattice.nil)
        yield result

    def stepAndPrint(exp: SchemeExp): A[Val] =
        for
            _ <- suspend(())
            result <- super.eval(exp)
        yield result

    /*def loop(s: Suspend): Int =
        println(s"current s $s")
        s match
            case Done(v) => v
            case s: SuspendInfo[_] =>
                println(s"Current state: ${s.state}")
                println("Continue (c) or quit (q)")
                val choice = StdIn.readLine
                if choice == "c" then loop(s.continue)
                else unit(lattice.nil)*/

    def stepNext(): Unit =
        isStep = true
        contin()
    def stepUntilNextBreak(): Unit =
        isStep = false
        contin()
