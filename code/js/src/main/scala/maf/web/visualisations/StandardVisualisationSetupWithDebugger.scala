package maf.web.visualisations

import maf.modular.scheme.modflocal._
import maf.modular.worklist._
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.language.scheme._

import scala.scalajs.js.annotation._
import maf.modular.scheme.modf.SchemeModFComponent.Main
import maf.modular.scheme.modf.SchemeModFComponent.Call
import org.scalajs.dom.raw.HTMLElement

class SimpleWebVisualisationWithDebugger(
    override val analysis: WebVisualisationAnalysis[_],
    width: Int,
    height: Int)
    extends WebVisualisation(width, height)

trait StandardVisualisationSetupWithDebugger extends VisualisationSetupWithDebugger:

    type Analysis = WebVisualisationAnalysis[_]

    private var visualisation: WebVisualisation = _

    def createVisualisation(
        analysis: Analysis,
        width: Int,
        height: Int
      ) =
        visualisation = new SimpleWebVisualisationWithDebugger(analysis, width, height)
        visualisation.node

    def setupStoreVisualisation(container: HTMLElement): Unit =
        visualisation.enableStoreVisualisation(container)

@JSExportTopLevel("standardModFVisualisationSetupWithDebugger")
object StandardModFVisualisationSetupWithDebugger extends StandardVisualisationSetupWithDebugger:

    def createAnalysis(text: String): Analysis =
        val program = SchemeParser.parseProgram(text)
        println("test")
        println(s"Program $program")
        val anl = new SimpleSchemeModFAnalysis(program)
            with SchemeModFNoSensitivity
            with SchemeConstantPropagationDomain
            with FIFOWorklistAlgorithm[SchemeExp]
            with WebVisualisationAnalysis[SchemeExp] {
            override def intraAnalysis(cmp: SchemeModFComponent) =
                new IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra
            // TODO: move this to somewhere in `maf.modular` directly
            type Module = Option[SchemeLambdaExp]
            def module(cmp: Component) = cmp match
                case Main                 => None
                case Call((lambda, _), _) => Some(lambda)
            def moduleName(mdl: Module) = mdl.map(_.lambdaName).getOrElse("main")
        }
        anl.init()
        anl

@JSExportTopLevel("standardModFLocalVisualisationSetupWithDebugger")
object StandardModFLocalVisualisationSetupWithDebugger extends StandardVisualisationSetupWithDebugger:
    def createAnalysis(txt: String): Analysis = ???
