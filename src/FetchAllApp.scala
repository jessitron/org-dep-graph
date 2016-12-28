import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import scala.xml.XML

object FetchAllApp extends App {

  import sys.process._

  val StartingProject = "rug-cli"
  val GitHubUrl = "git@github.com-personal:atomist/"
  val MavenGroup = "com.atomist"
  val OutputName = "atomist"


  type AtomistProject = String

  /**
    * Clone a repo in the current directory
    *
    * @return the cloned directory as a File
    */
  def bringDown: AtomistProject => File = { project =>
    // idempotent
    val existingDir = new File(project)
    if (existingDir.isDirectory)
      existingDir
    else {
      val gitUrl = s"$GitHubUrl$project"
      val exitCode = Seq("git", "clone", gitUrl).!
      if (exitCode != 0) throw new RuntimeException(s"git clone $gitUrl failed with $exitCode")
      val dir = new File(project)
      if (!dir.isDirectory) throw new RuntimeException(s"git clone $gitUrl did not produce directory $project")
      dir
    }
  }

  def atomistDependencies: File => Seq[AtomistProject] = { projectDir : File =>
    val pom = projectDir.listFiles().toList.find(_.getName == "pom.xml").getOrElse { throw new RuntimeException(s"No pom.xml found in ${projectDir.getName}")}
    val projectXml = XML.loadFile(pom)
    val deps = projectXml \ "dependencies" \ "dependency"
    val atomistDeps = deps.filter(node => (node \ "groupId").text == MavenGroup)
    atomistDeps.map(_ \ "artifactId").map(_.text)
  }

  val dependenciesOf: AtomistProject => Seq[AtomistProject] = bringDown andThen atomistDependencies

  type AtomistDependency = (AtomistProject, AtomistProject)

  def atomistDependencies(startingProject: AtomistProject): Seq[AtomistDependency] ={

    def go(allDeps: Map[AtomistProject, Seq[AtomistProject]], investigate: List[AtomistProject]): Map[AtomistProject, Seq[AtomistProject]] = {
      if(investigate.isEmpty)
        allDeps
    else
        {
          val next :: rest = investigate
          // could optimize by checking whether next is already in the map
          val deps = dependenciesOf(next)
          go(allDeps + (next -> deps), rest ++ deps)
        }
    }

    val allDeps = go(Map(), List(startingProject))

    allDeps.toSeq.flatMap { case (parent, deps) => deps.map(parent -> _) }
  }


  val edges = atomistDependencies(StartingProject).map(GraphViz.tupleToEdge)
  val r = GraphViz.makeAPicture(OutputName, edges, GraphViz.dashesToUnderscores)
  println(s"There is a picture for you in ${r.getName}")

}

object GraphViz {

  def tupleToEdge[A](tuple: (A,A)): Edge[A] = {
    new Edge[A] {
      val parent = tuple._1
      val child = tuple._2
      val label = None
    }
  }

  trait Edge[A] {
    def parent: A
    def child: A
    def label: Option[String]
  }

  def dag[A](name: String, edges: Seq[Edge[A]], toGraphVizId: A => String): String = {
    val formattedEdges = edges.map(formatEdge(toGraphVizId))
    s"digraph $name " + formattedEdges.mkString("{\n", "\n", "\n}")
  }

  def formatEdge[A](toGraphVizId: A => String)(e: Edge[A]): String =
    s"${toGraphVizId(e.parent)} -> ${toGraphVizId(e.child)};"

  def dashesToUnderscores(in: String): String = {
    in.replace('-','_')
  }

  import sys.process._

  def makeAPicture[A](name: String, edges: Seq[Edge[A]], toGraphVizId: A => String): File = {
    val dotText = dag(name, edges, toGraphVizId)
    val dotFile = s"$name.dot"
    Files.write(Paths.get(dotFile), dotText.getBytes(StandardCharsets.UTF_8))

    val outputFormat = "png"
    val pictureFile = s"$name.$outputFormat"
    val exitCode = Seq("dot", s"-T${outputFormat}", dotFile, "-o", pictureFile).!
    if (exitCode != 0) throw new RuntimeException("Failure running dot ... do you have graphviz installed?")
    new File(pictureFile)
  }
}
