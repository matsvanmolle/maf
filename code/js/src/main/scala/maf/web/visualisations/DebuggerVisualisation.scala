package maf.web.visualisations

import maf.core.Address
import maf.core.worklist.{FIFOWorkList, WorkList}
import maf.modular.scheme.monadic.{MonadFix, SimpleModFAnalysis, StateKeeper}
import maf.util.benchmarks.{Timeout, Timer}

import concurrent.duration.DurationInt
import maf.language.scheme.*
import maf.modular.Dependency
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.*
import org.scalajs.dom.*
import maf.web.utils.*
import maf.web.utils.D3Helpers.*

import scala.scalajs.js.annotation.*
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}


class DebuggerAnalysis(program: SchemeExp) extends SimpleModFAnalysis(program):

  var anlalys = new SimpleModFAnalysis(program)
  stateKeeper = new StateKeeper(anlalys)
  var webvis: DebuggerWebVisualisation = _

  var dependenciesMap: Map[Component, Set[Component]] = Map().withDefaultValue(Set())
  var readDependencies: Map[Component, Set[Component]] = Map().withDefaultValue(Set())
  var writeEffectsMap: Map[Component, Set[Address]] = Map().withDefaultValue(Set())



  def continue(step: Boolean): Unit =
    webvis.beforeStep()
    anlalys.loop(step)
    stateKeeper.newState()
    dependenciesMap = dependenciesMap + (anlalys.effectsState.cmp.asInstanceOf[Component] -> anlalys.effectsState.C.asInstanceOf[Set[Component]])
    readDependencies = anlalys.effectsState.R.asInstanceOf[Map[Component, Set[Component]]]
    writeEffectsMap =
      if anlalys.effectsState != null
      then
         writeEffectsMap + (anlalys.effectsState.cmp.asInstanceOf[Component] -> anlalys.effectsState.W.asInstanceOf[Set[Address]])
      else
          writeEffectsMap

    webvis.afterStep()
    webvis.refresh()
    //webvis.refreshWorklistVisualisation()
    //webvis.refreshStoreVisualisation()
    if anlalys.isFinisched then
      anlalys.printResult
      DebuggerVisualisation.stepButton.innerText = "Reset"
      DebuggerVisualisation.stepUntilBreakButton.classList.add("hidden")
      DebuggerVisualisation.stepClick = () => DebuggerVisualisation.reload()

  def startAnalysis() =
    anlalys.makeAnalysis
    webvis.node
    anlalys.loop(true)
    continue(false)


  def store: Map[maf.core.Address, Value] = if anlalys.effectsState == null
    then Map()
    else
      println("+++++ stor")
      anlalys.effectsState.sto.content
  def workList: FIFOWorkList[Component] = if anlalys.effectsState == null
    then null
    else anlalys.effectsState.wl.asInstanceOf[FIFOWorkList[Component]]
  def dependencies(cmp : Component): Set[Component] =
    println(s"-------- ${dependenciesMap}")
    dependenciesMap.getOrElse(cmp, Set())
  //def readDependencies: Set[Component] = anlalys.effectsState.R.asInstanceOf[Set[Component]]
  def writeEffects: Map[Component, Set[Address]] = writeEffectsMap //anlalys.effectsState.W
  def visited: Set[Component] =
    println(s"++++ ${anlalys.effectsState}")
    if anlalys.effectsState == null then
      Set()
    else
      println(s"+++++++ ${anlalys.effectsState.seen}")
      anlalys.effectsState.seen.asInstanceOf[Set[Component]]


class DebuggerVisualisation1(
                              override val analysis: DebuggerAnalysis,
                              width: Int,
                              height: Int)
  extends DebuggerWebVisualisation(width, height)

object ExamplePrograms3:
  val factorial: String =
    """|(define (factorial n)
       | (if (= n 0)
       |      1
       |      (* n (factorial (- n 1)))))
       |(factorial 5)""".stripMargin

@JSExportTopLevel("debuggerVisualisation")
object DebuggerVisualisation:

  var input: EditText = _
  var stepButton: html.Element = _
  var stepUntilBreakButton: html.Element = _
  var storeVisualisation: HTMLElement = _


  def onClick(): Unit = println("klik")
  var stepClick = () => println("click")
  var stepUntilBreakClick = () => println("click")
  var removeStore = () => ()

    


  protected def loadFile(program: String): Unit =

    stepButton.innerText = "Next"
    stepButton.classList.remove("hidden")
    stepButton.classList.remove("disabled")

    stepUntilBreakButton.innerText = "Step Until Next Breakpoint"
    stepUntilBreakButton.classList.remove("hidden")
    stepUntilBreakButton.classList.remove("disabled")

    // create an analysis
    //val analysis = createAnalysis(program)
    // remove the old visualisation if present
    /*this.webvis.foreach {
      document.querySelector(".visualisation").removeChild(_)
    }*/
    // remove the old store visualisation
    //storeVisualisation.innerHTML = ""
    // create a new visualisation
    //val width = document.querySelector(".visualisationContainer").asInstanceOf[HTMLElement].offsetWidth.asInstanceOf[Int]
    //val height = document.querySelector(".visualisationContainer").asInstanceOf[HTMLElement].offsetHeight.asInstanceOf[Int]
    //val webvis = createVisualisation(analysis, width, height)
    //setupStoreVisualisation(storeVisualisation)
    // load it in the main web page HTML
    //document.querySelector(".visualisation").appendChild(webvis)
    // update the state of the visualisation setup
    //current = Some((analysis, webvis))

    var parsedProgram = SchemeParser.parseProgram(program)
    val viz = new DebuggerVisualisation1(new DebuggerAnalysis(parsedProgram), 500, 500)
    viz.analysis.webvis = viz
    document.querySelector(".visualisation").appendChild(viz.node)
    viz.enableStoreVisualisation(storeVisualisation)

    /*def continue(step: Boolean): Unit =
      anl.loop(step)
      if anl.isFinisched then
        anl.printResult
        stepButton.innerText = "Reset"
        stepUntilBreakButton.classList.add("hidden")
        stepClick = () =>  reload()*/



    stepClick = () => viz.analysis.continue(true)
    stepUntilBreakClick = () => viz.analysis.continue(false)
    viz.analysis.startAnalysis()
    
    
    


  def reload(): Unit =
    stepButton.classList.add("btn")
    stepButton.classList.add("hidden")
    stepUntilBreakButton.classList.add("btn")
    stepUntilBreakButton.classList.add("hidden")
    removeStore()
    input.reset()

    
   


  @JSExport
  def setup(): Unit =
    // add element to provide a program to the analysis
    input = EditText(loadFile)
    input.setFile(ExamplePrograms3.factorial)
    document.body.appendChild(input.render())

    stepButton = Button("Click 'Start Analysis' to start.")(stepClick())
    stepButton.classList.add("btn")
    stepButton.classList.add("hidden")
    input.appendChild(stepButton)

    stepUntilBreakButton = Button("Click 'Start Analysis' to start.")(stepUntilBreakClick())
    stepUntilBreakButton.classList.add("btn")
    stepUntilBreakButton.classList.add("hidden")
    input.appendChild(stepUntilBreakButton)

    // Add the container that holds both the graph visualisation
    // as well as the store visualisation
    val container = document.createElement("div")
    container.setAttribute("id", "visualisationContainer")
    document.body.appendChild(container)

    // Add the container for holding the store visualisation
    storeVisualisation = document.createElement("div").asInstanceOf[HTMLElement]
    storeVisualisation.setAttribute("id", "storeVisualisation")
    container.appendChild(storeVisualisation)
    removeStore = () =>
      container.removeChild(storeVisualisation)
      storeVisualisation = document.createElement("div").asInstanceOf[HTMLElement]
      storeVisualisation.setAttribute("id", "storeVisualisation")
      container.appendChild(storeVisualisation)
      println("remove")

    // Add the visualisation div
    val div = document.createElement("div")
    div.classList.add("visualisation")
    container.appendChild(div)





    // Add the container that holds both the graph visualisation
    // as well as the store visualisation
    //val container = document.createElement("div")
    //container.setAttribute("id", "visualisationContainer")
    //document.body.appendChild(container)

    // Add the container for holding the store visualisation
    //storeVisualisation = document.createElement("div").asInstanceOf[HTMLElement]
    //storeVisualisation.setAttribute("id", "storeVisualisation")
    //container.appendChild(storeVisualisation)
    
    val body = d3.select(document.body)
    
    
    
    
    
    
