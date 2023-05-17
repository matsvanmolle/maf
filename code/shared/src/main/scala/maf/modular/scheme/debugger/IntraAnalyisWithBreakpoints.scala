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
    this: SchemeDomain with SchemeModFLocalSensitivity =>

    var contin = () => println("noting to do!")
    var isStep: Boolean = false;
    var stateKeeper: StateKeeper = _

    import analysisM_.*

    type A[X] = SuspendM[X]

    import maf.language.scheme
    override def eval(exp: SchemeExp): A[Val] =
        println(s"eval $exp")
        exp match
            case DebuggerBreak(pred, _) =>
                println(SchemeInterpreterDebugger.evalPredicate(pred, stateKeeper))
                breakAndPrint()
            case _ =>
                if isStep
                then
                    isStep = false
                    stepAndPrint(exp)
                else
                    super.eval(exp)



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

