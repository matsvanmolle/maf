package maf.cli.experiments.preprocessing

import maf.language.scheme.*
import maf.bench.scheme.*
import maf.util.Reader
import maf.core.Identity
import scala.util.Try

object DefineTester extends UndefinerTester:

    def main(args: Array[String]): Unit =
      if args.size != 1 then println("Usage: DefineTester benchmarks")
      else
          val directory = args(0)
          val programs = SchemeBenchmarkPrograms.fromFolderR(directory)(".DS_Store")
          val parsed = programs.map(name => (Reader.loadFile(name), name)).flatMap { (s, name) =>
            Try(SchemeParser.parseProgramText(s)).toOption.map((e) => (e, name))
          }
          val results = parsed.map { case (e, name) =>
            (check(e, true), name)
          }

          for (result, program) <- results do
              if result.isError then println(s"Program $program contains defines on invalid locations, ${result.show}")

end DefineTester
