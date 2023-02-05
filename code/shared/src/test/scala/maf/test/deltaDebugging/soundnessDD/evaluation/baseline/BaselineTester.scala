package maf.test.deltaDebugging.soundnessDD.evaluation.baseline

import maf.core.Identity
import maf.language.scheme.SchemeExp
import maf.language.scheme.interpreter.ConcreteValues.Value
import maf.test.SlowTest
import maf.test.deltaDebugging.soundnessDD.SoundnessDDTester
import maf.util.Reader

trait BaselineTester extends SoundnessDDTester {
  
  val bugName: String

  override def onBenchmark(benchmark: Benchmark): Unit =
    property(s"Analysis of $benchmark using $name is sound.", SlowTest) {
      // load the benchmark program
      val content = Reader.loadFile(benchmark)
      val program = parseProgram(content, benchmark)

      runAndCompare(program, benchmark) match
        case Some(failureMsg) =>
          if failureMsg.nonEmpty then
            BaselineDD.bugName = bugName
            BaselineDD.reduce(program, this, benchmark)
        case _ =>
    }
}
