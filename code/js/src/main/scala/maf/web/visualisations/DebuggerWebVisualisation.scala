package maf.web.visualisations

import maf.core.*
import maf.core.worklist.FIFOWorkList
import maf.modular.*
import maf.modular.worklist.SequentialWorklistAlgorithm
import maf.util.benchmarks.Timeout
import maf.web.utils.JSHelpers.*
import maf.web.utils.D3Helpers.*
import maf.web.utils.*

// Scala.js-related imports
import scala.scalajs.js
import org.scalajs.dom.document

// null values are used here due to JS interop
import scala.language.unsafeNulls
import maf.modular.scheme.PrmAddr
import org.scalajs.dom.raw.HTMLElement
import maf.modular.scheme.VarAddr

object DebuggerWebVisualisation:
    // some constants
    val __CIRCLE_RADIUS__ = 15
    val __SVG_ARROW_ID__ = "endarrow"
    val __CSS_NOT_VISITED__ = "not_visited"
    val __CSS_IN_WORKLIST__ = "in_worklist"
    val __CSS_NEXT_COMPONENT__ = "next_component"
    val __FORCE_COLLIDE__ = "collide"
    val __FORCE_CHARGE__ = "charge"
    val __FORCE_LINKS__ = "links"
    val __FORCE_CENTER__ = "center"

abstract class DebuggerWebVisualisation(width: Int, height: Int):

    import DebuggerWebVisualisation._

    val analysis: DebuggerAnalysis

    // give the analysis a pointer to this webvis
    analysis.webvis = this

    // TODO: make these abstract
    def componentText(cmp: analysis.Component): String = cmp.toString
    def componentKey(cmp: analysis.Component): Any = Some(RED)

    //
    // STORE VISUALISATION
    //

    /** Represent changes in the store */
    enum Change:
        /** A new entry in the store */
        case Nww(adr: Address, vlu: analysis.Value)

        /** An updated entry in the store * */
        case Upd(adr: Address, oldVlu: analysis.Value, vlu: analysis.Value)

    private def diff(old: Map[Address, analysis.Value], nww: Map[Address, analysis.Value]): List[Change] =
        nww.keys.foldLeft(List[Change]())((changes, newKey) =>
            import Change.*
            (if old.contains(newKey) && old(newKey) != nww(newKey) then
                 // an update
                 Upd(newKey, old(newKey), nww(newKey))
             else
                 // a new key
                 Nww(newKey, nww(newKey))
            ) :: changes
        )
    private var storeTable: HtmlTable[(String, String)] = _
    private var worklistTable: HtmlList = _
    private var wlContainer: HTMLElement = _

    private var lastStore: Map[Address, analysis.Value] = Map()
    private var lastWorkList: FIFOWorkList[analysis.Component] = FIFOWorkList.empty

    def refreshStoreVisualisation(): Unit =
        import Change.*
        // compute the difference between the last know store and the new store
        val newStore = analysis.store.view.filterKeys {
            case PrmAddr(_) => false
            case _          => true
        }.toMap

        val dff = diff(lastStore, newStore)
        // reset classes for each row
        storeTable.classed { case _ =>
            ""
        }
        println(dff)
        // add any new rows
        dff.foreach {
            case Nww(adr, vlu) => storeTable.addRow((adr.toString, vlu.toString), Some("added"))
            case Upd(adr, oldVlu, vlu) =>
                storeTable.update((adr.toString, oldVlu.toString), (adr.toString, vlu.toString), Some("updated"))
        }

    def getAddedElements(oldList: List[Any], newList: List[Any]): List[Any] = {
        val commonPrefixLength = oldList.zip(newList).prefixLength { case (a, b) => a == b }
        val addedElements = newList.drop(commonPrefixLength)
        addedElements
    }

    def refreshWorklistVisualisation(): Unit =
        // add any new rows
        worklistTable.dropTable(wlContainer)
        worklistTable.render(wlContainer)
        analysis.workList.toList.foreach { element =>
            worklistTable.addRow(element.toString)
        }
        worklistTable.render(wlContainer)


    def enableWorklistVisualisation(container: HTMLElement): Unit =
        worklistTable = new HtmlList("worklist")
        wlContainer = container
        worklistTable.render(container)
        worklistTable.addRow("test", Some("added"))
        worklistTable.render(container)
        worklistTable.dropTable(container)
        worklistTable.render(container)

    def enableStoreVisualisation(container: HTMLElement): Unit =
        storeTable = HtmlTable(List("Address", "Value"))
        storeTable.render(container)

    //
    // COLOR WHEEL
    //

    var colorWheel: Map[Any, JsAny] = Map()
    def colorFor(cmp: analysis.Component): JsAny = colorForKey(componentKey(cmp))
    def colorForKey(key: Any): JsAny = colorWheel.get(key) match
        case None =>
            val newColor = randomColor()
            colorWheel += (key -> newColor)
            newColor
        case Some(existingColor) => existingColor

    //
    // BOOKKEEPING (needed to play nice with Scala.js and d3.js)
    //

    var edgeID: Int = 0 // Last unused edge id.
    def newEdgeID(): String =
        val id = s"edge$edgeID"
        edgeID += 1
        id

    // TODO: find a better interface
    trait Node extends js.Object:
        def displayText(): String
        def data(): Any

    object Node:
        def apply(v: analysis.Component | Address): Node = v match
            case adr: Address =>
                println(s"=== adr node $adr")
                AdrNode(adr)
            case cmp: analysis.Component @unchecked =>
                println(s"=== cmpNode $cmp")
                CmpNode(cmp)
    class CmpNode(val component: analysis.Component) extends Node:
        def displayText(): String = componentText(component)
        def data(): Any = component
    class AdrNode(val addr: Address) extends Node:
        def displayText(): String = addr.toString()
        def data(): Any = addr

    object IsCmpNode:
        def unapply(v: js.Object): Option[CmpNode] =
            if v.isInstanceOf[CmpNode] then Some(v.asInstanceOf[CmpNode]) else None
    object IsAdrNode:
        def unapply(v: js.Object): Option[AdrNode] =
            if v.isInstanceOf[AdrNode] then Some(v.asInstanceOf[AdrNode]) else None

    // An edge contains an id so it can be referenced (e.g., to add text).
    class Edge(val source: Node, val target: Node) extends js.Object:
        val id: String = newEdgeID() // Linking errors arise when adding this as a class argument...

    var nodesData: Set[Node] = Set()
    var edgesData: Set[Edge] = Set()
    // TODO: use weak maps here to prevent memory leaks?
    var nodesColl: Map[analysis.Component | Address, Node] = Map()
    var edgesColl: Map[(Node, Node), Edge] = Map()

    def getNode(cmp: analysis.Component | Address): Node = nodesColl.get(cmp) match
        case None =>
            val newNode = Node(cmp)
            nodesColl += (cmp -> newNode)
            newNode
        case Some(existingNode) => existingNode

    def getEdge(source: Node, target: Node, kind: EdgeKind = EdgeKind.Call): Edge = edgesColl.get((source, target)) match
        case None =>
            val newEdge = kind match
                case EdgeKind.Read =>
                    val newEdge = new Edge(target, source)
                    edgesColl += ((target, source) -> newEdge)
                    newEdge
                case _ =>
                    val newEdge = new Edge(source, target)
                    edgesColl += ((source, target) -> newEdge)
                    newEdge
            newEdge

        case Some(existingEdge) => existingEdge

    //
    // VISUALISATION SETUP
    //

    // setup the svg and visualisation skeleton
    val node = document.createElement("div")
    protected val svgDiv = document.createElement("div")
    node.appendChild(svgDiv)
    protected val svgNode = document.createElementNS("http://www.w3.org/2000/svg", "svg")
    svgDiv.appendChild(svgNode)
    protected val svg = d3.select(svgNode).attr("width", width).attr("height", height)
    private val outerContainer = svg.append("g")
    private val innerContainer = outerContainer.append("g").attr("transform", s"translate(${width / 2},${height / 2})")
    // augment the svg capabilities
    setupMarker(svg) // <- this allows us to use a fancy arrow in the svg
    svg.call(
      d3.zoom()
          .on("zoom",
              () => { // <- this sets up a fancy zoom effect
                  outerContainer.attr("transform", d3.event.transform)
              }
          )
    )
    // setup the nodes infrastructure
    private val nodesContainer = innerContainer.append("g").attr("class", "nodes")
    protected var nodes = nodesContainer.selectAll("g")
    // setup the edges infrastructure
    private val edgesContainer = innerContainer.append("g").attr("class", "links")
    protected var edges = edgesContainer.selectAll("path")
    // setup the labels infrastructure
    private val labelsContainer = innerContainer.append("g").attr("class", "labels")
    protected var labels = labelsContainer.selectAll("label")
    // setup the simulation
    private val simulation = d3.forceSimulation()
    simulation
        .force(__FORCE_COLLIDE__, d3.forceCollide().radius(__CIRCLE_RADIUS__))
        .force(__FORCE_CHARGE__, d3.forceManyBody().strength(-1000))
        .force(__FORCE_LINKS__, d3.forceLink().distance(150))
        .force(__FORCE_CENTER__, d3.forceCenter())
        .on("tick", () => onTick())

    // Adds a new base marker to the given svg. The marker is returned, so extra attributes can be added later.
    protected def newMarker(svg: JsAny, id: String) =
        // adapted from http://bl.ocks.org/fancellu/2c782394602a93921faff74e594d1bb1
        val marker: js.Dynamic = svg
            .append("defs")
            .append("marker")
            .attr("id", id)
            .attr("viewBox", "-0 -5 10 10")
            .attr("refX", 0)
            .attr("refY", 0)
            .attr("orient", "auto")
            .attr("markerWidth", 5)
            .attr("markerHeight", 5)
        marker
            .append("svg:path")
            .attr("d", "M 0,-5 L 10 ,0 L 0,5")
        marker

    protected def setupMarker(svg: JsAny) = newMarker(svg, __SVG_ARROW_ID__)

    //.attr("fill", "#999")
    //.style("stroke","none")

    def onTickHook(): Unit = ()

    private def onTick() =
        // update the nodes
        nodes.attr("transform", (node: JsAny) => s"translate(${node.x},${node.y})")
        // update the edges
        try
            edges.attr(
              "d",
              (edge: JsAny) =>
                  if edge.source == edge.target then {
                      val cx = edge.source.x.asInstanceOf[Double]
                      val cy = edge.source.y.asInstanceOf[Double]
                      val x1 = cx - __CIRCLE_RADIUS__
                      val y1 = cy
                      val x2 = cx - 9
                      val y2 = cy - __CIRCLE_RADIUS__ - 8
                      s"M$x1 $y1 A ${__CIRCLE_RADIUS__} ${__CIRCLE_RADIUS__} 1 1 1 $x2 $y2"
                  } else {
                      val sourceX = edge.source.x.asInstanceOf[Double]
                      val sourceY = edge.source.y.asInstanceOf[Double]
                      val targetX = edge.target.x.asInstanceOf[Double]
                      val targetY = edge.target.y.asInstanceOf[Double]
                      val deltaX = targetX - sourceX
                      val deltaY = targetY - sourceY
                      val dist = Math.sqrt((deltaX * deltaX) + (deltaY * deltaY))
                      val scaleFactorSource = __CIRCLE_RADIUS__ / dist
                      val scaleFactorTarget = (__CIRCLE_RADIUS__ + 10) / dist
                      val x1 = sourceX + (deltaX * scaleFactorSource)
                      val x2 = targetX - (deltaX * scaleFactorTarget)
                      val y1 = sourceY + (deltaY * scaleFactorSource)
                      val y2 = targetY - (deltaY * scaleFactorTarget)
                      s"M$x1 $y1 L$x2 $y2"
                  }
            )
        catch case e => println(s"error $e")
        // Maybe perform other updates.
        // TODO: just override the existing onTick method (using super.onTick())?
        onTickHook()

    //
    // REFRESHING
    //

    // updates both the data and the visualisation
    def refresh(): Unit =
        refreshData()
        refreshVisualisation()

    // Ensures that `nodesData` and `edgesData` are in sync with the analysis.
    // Allows deleted components to be removed from the visualiation.
    def refreshData(): Unit =
        // refresh the nodes
        nodesData = Set.empty[Node]
        analysis.visited.foreach { cmp =>
            val node = getNode(cmp)
            nodesData += node

            val targets: Set[analysis.Component | Address] = analysis.dependencies(cmp) ++ analysis.readDependencies(cmp).filter {
                case PrmAddr(_) => false
                case _          => true
            } ++ analysis.writeEffects(cmp).filter {
                case PrmAddr(_) => false
                case _          => true
            }

            targets.foreach { target =>
                val targetNode = getNode(target)
                val edge = getEdge(node, targetNode)
                nodesData += targetNode
                edgesData += edge
            }
        }

    // More efficient than `refreshData`: updates only data that may have changed after stepping.
    protected var prevComponent: analysis.Component = _
    private var prevCalls: Set[analysis.Component] = _

    def beforeStep(): Unit =
        println("worklist")
        println(analysis.workList)
        lastStore = analysis.store
        if analysis.workList != null && analysis.workList.nonEmpty then
            prevComponent = analysis.workList.head
            prevCalls = analysis.dependencies(prevComponent)

    def afterStep(): Unit =
        // refresh the data
        if prevComponent != null then refreshDataAfterStep()
        // refresh the visualisation
        refreshVisualisation()
        // refresh the visualisation for the store
        refreshStoreVisualisation()
        // refreh wl vis
        refreshWorklistVisualisation()

    enum EdgeKind:
        case Call
        case Read
        case Write

    protected def refreshDataAfterStep(): Unit =
        println("refresh data")
        val sourceNode = getNode(prevComponent)
        nodesData += sourceNode
        prevCalls.foreach { otherCmp =>
            val targetNode = getNode(otherCmp)
            val edge = getEdge(sourceNode, targetNode)
            edgesData -= edge
        }
        // add the new edges
        val targets: Set[(analysis.Component | Address, EdgeKind)] =
            analysis.dependencies(prevComponent).map((_, EdgeKind.Call)) ++ analysis
                .readDependencies(prevComponent)
                .filter {
                    case PrmAddr(_) => false
                    case _          => true
                }
                .map((_, EdgeKind.Read)) ++ analysis
                .writeEffects(
                  prevComponent
                )
                .filter {
                    case PrmAddr(_) => false
                    case _          => true
                }
                .map((_, EdgeKind.Write))
        println(s"refreshData $targets")
        targets.foreach { case (otherCmp, kind) =>
            val targetNode = getNode(otherCmp)
            println(s"=== targetNode $targetNode")
            val edge = getEdge(sourceNode, targetNode, kind)
            nodesData += targetNode
            edgesData += edge
        }

    // Can be overridden to do perform extra updates upon the refresh of the visualisation.
    // TODO: just override existing method?
    def refreshHook(): Unit = ()

    // updates the visualisation: draws all nodes/edges, sets correct CSS classes, etc.
    def refreshVisualisation(): Unit =
        println("update")
        // update the nodes
        val nodesUpdate = nodes.data(nodesData, (n: Node) => n.data())
        println("node update")
        println(nodesUpdate)
        val newGroup = nodesUpdate
            .enter()
            .append("g")
            .call(dragEffect)
        newGroup
            .append("circle")
            .attr("r", __CIRCLE_RADIUS__)
        newGroup
            .append("text")
            .attr("dx", __CIRCLE_RADIUS__)
            .attr("dy", __CIRCLE_RADIUS__)
        nodes = newGroup.merge(nodesUpdate)
        nodes
            .select("text")
            .text((node: Node) => node.displayText())
        nodesUpdate.exit().remove()
        classifyNodes()
        // update the edges
        val edgesUpdate = edges.data(edgesData, (e: Edge) => (e.source.data(), e.target.data()))
        edges = edgesUpdate.enter().append("path").merge(edgesUpdate)
        edges.attr("id", (e: Edge) => e.id)
        classifyEdges()
        edgesUpdate.exit().remove()
        // possibly perform more updates
        refreshHook()
        classifyLabels()
        // update the simulation
        simulation.nodes(nodesData)
        simulation.force(__FORCE_LINKS__).links(edgesData)
        simulation.alpha(1).restart()

    /** Classifies every node based on its role in the analysis, so the node can be coloured correctly. */
    def classifyNodes(): Unit =
        nodes
            // Apparently the Scala compiler does not just accept the cases as anonymous function, hence the longer implementation.
            .classed(
              __CSS_IN_WORKLIST__,
              (node: Node) =>
                  node match {
                      case IsCmpNode(node) => analysis.workList.toSet.contains(node.component)
                      case _               => false
                  }
            )
            .classed(
              __CSS_NOT_VISITED__,
              (node: Node) =>
                  node match {
                      case IsCmpNode(node) => !analysis.visited.contains(node.component)
                      case _               => false
                  }
            )
            .classed(
              __CSS_NEXT_COMPONENT__,
              (node: Node) =>
                  node match {
                      case IsCmpNode(node) => analysis.workList.toList.headOption.contains(node.component)
                      case _               => false
                  }
            )
            .style(
              "fill",
              (node: Node) =>
                  node match {
                      case IsCmpNode(node) =>
                          GREEN
                      case IsAdrNode(node) =>
                          BLUE
                      case _ => false
                  }
            )

    /** Classifies every edge based on its role in the analysis, so the edge can be coloured correctly. */
    def classifyEdges(): Unit = ()
    def classifyLabels(): Unit = ()

    //
    // DRAGGING
    //

    // create a fancy drag effect
    val dragEffect = d3
        .drag()
        .on("start", (node: JsAny) => onDragStart(node))
        .on("drag", (node: JsAny) => onDragDrag(node))
        .on("end", (node: JsAny) => onDragEnd(node))

    private def onDragStart(node: JsAny): Unit =
        val isActive = d3.event.active.asInstanceOf[Int]
        if isActive == 0 then simulation.alphaTarget(0.3).restart()
        node.fx = node.x
        node.fy = node.y
    private def onDragDrag(node: JsAny): Unit =
        node.fx = d3.event.x
        node.fy = d3.event.y
    private def onDragEnd(node: JsAny): Unit =
        val isActive = d3.event.active.asInstanceOf[Int]
        if isActive == 0 then simulation.alphaTarget(0)
        node.fx = null
        node.fy = null

    refresh()
