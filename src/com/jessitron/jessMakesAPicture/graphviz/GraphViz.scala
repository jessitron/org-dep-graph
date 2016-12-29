package com.jessitron.jessMakesAPicture.graphviz

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}



object GraphViz {

  def tupleToEdge[A](tuple: (A, A)): Edge[A] = {
    new Edge[A] {
      val parent = tuple._1
      val child = tuple._2
      val label = None
    }
  }

  def stringToNode(nodeId: String): Node = {
    new Node {
      val id = NodeId(dashesToUnderscores(nodeId))
    }
  }

  sealed trait LineStyle

  case object Dashed extends LineStyle

  case object Dotted extends LineStyle

  case class NodeId(id: String) {
    if (id.contains("-")) throw new IllegalArgumentException("No dashes allowed in the id")
  }

  trait Node {
    def id : NodeId
    final def idString : String = id.id
    def label : Option[String] = None
  }

  trait Edge[A] {
    def parent: A

    def child: A

    def label: Option[String]

    def style: Option[LineStyle] = None
  }

  def dag[A](name: String, edges: Seq[Edge[A]], toNode: A => Node): String = {
    val formattedEdges = edges.map(formatEdge(toNode))
    s"digraph $name " + formattedEdges.mkString("{\n", "\n", "\n}")
  }

  def formatEdge[A](toNode: A => Node)(e: Edge[A]): String = {
    val modifiers =
      e.label.map { l => s"""label="$l"""" } ++
        e.style.map { s => s"""style="${s.toString.toLowerCase}"""" }
    s"${toNode(e.parent).idString} -> ${toNode(e.child).idString} ${modifiers.mkString("[", ",", "]")};"
  }

  def dashesToUnderscores(in: String): String = {
    in.replace('-', '_')
  }

  import sys.process._

  def makeAPicture[A](name: String, edges: Seq[Edge[A]], toNode: A => Node): File = {
    val dotText = dag(name, edges, toNode)
    val dotFile = s"$name.dot"
    Files.write(Paths.get(dotFile), dotText.getBytes(StandardCharsets.UTF_8))

    val outputFormat = "png"
    val pictureFile = s"$name.$outputFormat"
    val exitCode = Seq("dot", s"-T${outputFormat}", dotFile, "-o", pictureFile).!
    if (exitCode != 0) throw new RuntimeException("Failure running dot ... do you have graphviz installed?")
    new File(pictureFile)
  }
}
