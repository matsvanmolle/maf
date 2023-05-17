package maf.web.utils

import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.raw.HTMLHtmlElement

import scala.scalajs.js
import org.scalajs.dom.document
import org.scalajs.dom.raw.HTMLLinkElement

/*trait ColumnExtract[T]:
    def extract(v: T): List[String]

object ColumnExtract:
    def apply[T: ColumnExtract]: ColumnExtract[T] = summon[ColumnExtract[T]]

    given tuple2[A, B]: ColumnExtract[(A, B)] with
        def extract(v: (A, B)): List[String] =
            List(v._1.toString, v._2.toString)

class HtmlList[T: String](header: List[String]):
    /** Initially no table has been rendered */
    private var renderedTable: Option[HTMLElement] = None

    /** A map of data points to rows in the table */
    private var renderedRows: List[Tuple2[T, HTMLElement]] = List()


    def addRow(data: T, cls: Option[String] = None): Unit =
            val renderedRow = document.createElement("tr").asInstanceOf[HTMLElement]
            data.foreach(col =>
                val renderedCol = document.createElement("td").asInstanceOf[HTMLElement]
                renderedCol.innerText = col
                renderedRow.appendChild(renderedCol)
            )
            renderedRows = renderedRows (data -> renderedRow)

            if cls.isDefined then renderedRow.className = cls.get
            renderedTable.get.appendChild(renderedRow)





    def render(container: HTMLElement): Unit = renderedTable match
        case Some(t) =>
            container.removeChild(t) // no-op if element is not there
            container.appendChild(t)
        case _ =>
            val table = document.createElement("table").asInstanceOf[HTMLLinkElement]
            val headerRow = document.createElement("tr")
            header.foreach(name =>
                val headerColumn = document.createElement("td")
                headerColumn.innerText = name
                headerRow.appendChild(headerColumn)
            )
            table.appendChild(headerRow)
            container.appendChild(table)
            renderedTable = Some(table)
*/