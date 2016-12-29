import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import GraphViz.{Dashed, Dotted, Edge}

import scala.xml.{Node, XML}

object FetchAllApp extends App {


  val StartingProject = "rug-cli"
  val MavenGroup = "com.atomist"
  val OutputName = "atomist"

  type InOrgProject = String //GOAL: case class InOrgProject(name: String, version: Version)
  type Version = String

  case class IntraOrgDependency(parent: InOrgProject, child: InOrgProject, version: Version, scope: Option[String]) extends Edge[InOrgProject] {
    val label = Some(scope match {
      case None => version
      case Some(sc) => s"$version ($sc)"
    })
    override def style =
      scope.map {
        case "test" => Dashed
        case "provided" => Dotted
      }
  }


  def dependenciesFromPom: File => Seq[IntraOrgDependency] = { projectDir: File =>
    val pom = projectDir.listFiles().toList.find(_.getName == "pom.xml").getOrElse {
      throw new RuntimeException(s"No pom.xml found in ${projectDir.getName}")
    }
    val projectXml = XML.loadFile(pom)
    val parentName = (projectXml \ "artifactId").text
    val deps = projectXml \ "dependencies" \ "dependency"
    val atomistDeps = deps.filter(node => (node \ "groupId").text == MavenGroup)

    def interpretDependencyNode(node: Node): IntraOrgDependency = {
      val childName = (node \ "artifactId").text
      val scope = (node \ "scope").text
      val version = (node \ "version").text
      IntraOrgDependency(parentName, childName, version, if (scope.isEmpty) scala.None else Some(scope))
    }

    atomistDeps.map(interpretDependencyNode)
  }

  val dependenciesOf: InOrgProject => Seq[IntraOrgDependency] = Git.bringDown andThen dependenciesFromPom


  def findAllDependencies(startingProject: InOrgProject): Seq[IntraOrgDependency] = {

    def go(allDeps: Map[InOrgProject, Seq[IntraOrgDependency]], investigate: List[InOrgProject]): Map[InOrgProject, Seq[IntraOrgDependency]] = {
      if (investigate.isEmpty)
        allDeps
      else {
        val next :: rest = investigate
        if(allDeps.contains(next))
          go(allDeps, rest)
        else {
          val deps = dependenciesOf(next)
          go(allDeps + (next -> deps), rest ++ deps.map(_.child))
        }
      }
    }

    val allDeps = go(Map(), List(startingProject))

    allDeps.toSeq.flatMap { case (_parent, deps) => deps }
  }


  val edges = findAllDependencies(StartingProject)
  val r = GraphViz.makeAPicture(OutputName, edges, GraphViz.stringToNode)
  println(s"There is a picture for you in ${r.getAbsolutePath}")

}

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

object Git {

  import sys.process._

  val GitHubUrl = "git@github.com-personal:atomist/"
  val FetchForUptodateness = false

  private def fetchIn(dir: File): File = {
    println(s"fetching in ${dir.getName}")
    val fetchExitCode = Seq("git", "-C", dir.getPath, "fetch").!
    if (fetchExitCode != 0) throw new RuntimeException(s"Could not fetch in ${dir.getPath}")
    dir
  }

  /**
    * Clone a repo in the current directory
    *
    * @return the cloned directory as a File
    */
  def bringDown: String => File = { project =>
    val existingDir = new File(project)
    if (existingDir.isDirectory) {
      if (FetchForUptodateness)
        fetchIn(existingDir)
      else
        existingDir
    }
    else {
      val gitUrl = s"$GitHubUrl$project"
      val exitCode = Seq("git", "clone", gitUrl).!
      if (exitCode != 0) throw new RuntimeException(s"git clone $gitUrl failed with $exitCode")
      val dir = new File(project)
      if (!dir.isDirectory) throw new RuntimeException(s"git clone $gitUrl did not produce directory $project")
      dir
    }
  }

}
