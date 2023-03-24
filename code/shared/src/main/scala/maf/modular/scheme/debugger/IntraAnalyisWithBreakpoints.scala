package maf.modular.scheme.debugger

import maf.modular.scheme.modflocal.SchemeSemantics
import maf.language.scheme.SchemeExp
import maf.modular.scheme.SchemeDomain
import maf.modular.scheme.modflocal.SchemeModFLocalSensitivity
import maf.language.scheme.*
import maf.core.Identifier
import maf.modular.scheme.monadic.*
import maf.core.Monad.MonadSyntaxOps
import scala.io.StdIn


import scala.annotation.tailrec

trait IntraAnalyisWithBreakpoints extends Monolith:
    this: SchemeDomain with SchemeModFLocalSensitivity =>

    var step: Boolean = false;

    import analysisM_.*

    type A[X] = SuspendM[X]

    import maf.language.scheme
    override def eval(exp: SchemeExp): A[Val] =
        println(s"eval $exp")
        exp match
            case DebuggerBreak(pred, _) => breakAndPrint()
                for
                    result <- eval(pred)
                    _ <- cond(result, breakAndPrint(), unit(lattice.nil))
                yield lattice.nil
            case _ =>
                if step
                then
                    step = false
                    stepAndPrint(exp)
                else
                    super.eval(exp)



    def breakAndPrint(): A[Val] =
        println("mynicebreak")
        for
            _ <- suspend(())
            _ = println("after suspend")

            result <- unit(lattice.nil)
        yield result

    def stepAndPrint(exp: SchemeExp): A[Val] =
        println("mynicestep")
        for
            _ <- suspend(())
            _ = println("after suspend")

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

    def step(idk: Any): Unit = println("step")