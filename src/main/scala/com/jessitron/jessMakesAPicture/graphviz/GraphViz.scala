package com.jessitron.jessMakesAPicture.graphviz

import java.io.{File, IOException}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}


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
      val id = NodeId(nodeId)
    }
  }

  sealed trait LineStyle

  case object Dashed extends LineStyle

  case object Dotted extends LineStyle

  case class NodeId(private val _id: String) {
    val id = dashesToUnderscores(_id)

    val idString = id
  }

  trait Node {
    def id: NodeId

    final def idString: String = id.id

    def label: String = idString
  }

  trait Edge[A] {
    def parent: A

    def child: A

    def label: Option[String]

    def style: Option[LineStyle] = None
  }

  private def dag[A](name: String, edges: Seq[Edge[A]], toNode: A => NodeId, nodesToLabel: Seq[Node]): String = {
    val formattedEdges = edges.map(formatEdge(toNode))
    val formattedNodes = nodesToLabel.map(formatNode).distinct
    s"digraph $name " + (formattedEdges ++ formattedNodes).mkString("{\n", "\n", "\n}")
  }

  private def formatEdge[A](toNode: A => NodeId)(e: Edge[A]): String = {
    val modifiers =
      e.label.map { l => s"""label="$l"""" } ++
        e.style.map { s => s"""style="${s.toString.toLowerCase}"""" }
    s"${toNode(e.parent).idString} -> ${toNode(e.child).idString} ${modifiers.mkString("[", ",", "]")};"
  }

  private def formatNode(node: Node): String = {
    s"""${node.idString} [label="${node.label}"];"""
  }

  private def dashesToUnderscores(in: String): String = {
    in.replace('-', '_')
  }

  import sys.process._

  def makeAPicture[A](name: String, edges: Seq[Edge[A]], toNode: A => NodeId, nodesToLabel: Seq[Node] = Seq()): File = {
    val dotText = dag(name, edges, toNode, nodesToLabel)
    val dotFile = writeDotFile(name, dotText)

    runDot("png", dotFile, name)
  }

  private def writeDotFile(name: String, contents: String): Path = {
    val dotFile = s"$name.dot"
    Files.write(Paths.get(dotFile), contents.getBytes(StandardCharsets.UTF_8))
  }

  private def runDot(outputFormat: String, dotFile: Path, outputName: String): File = {
    val pictureFile = s"$outputName.$outputFormat"
    val exitCode = try {
      Seq("dot", s"-T${outputFormat}", dotFile.toString, "-o", pictureFile).!
    } catch {
      case e: IOException =>
        throw new RuntimeException("Could not run dot. Do you have graphviz installed?", e)
    }

    if (exitCode != 0) {
      println(Files.readAllBytes(dotFile))
      throw new RuntimeException("Failure running dot.")
    }

    new File(pictureFile)
  }


}
