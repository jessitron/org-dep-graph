package com.jessitron.jessMakesAPicture

import com.jessitron.jessMakesAPicture.git.GitHubOrg
import com.jessitron.jessMakesAPicture.graphviz.GraphViz
import com.jessitron.jessMakesAPicture.graphviz.GraphViz.{Edge, LineStyle, NodeId}
import com.jessitron.jessMakesAPicture.maven.{BuildScript, Maven}
import com.jessitron.jessMakesAPicture.maven.Maven.{InOrgProject, IntraOrgDependency, ProjectName}

import scala.annotation.tailrec

object MakeAPicture extends App {

  val StartingProject = "rug-runner"
  val GitHubUrl = "git@github.com:atomisthq/"
  val MavenGroup = "com.atomist"
  val OutputName = "atomist"
  val BuildFileLocation = "bin/"

  val git = new GitHubOrg(GitHubUrl)

  def dependencyEdge: IntraOrgDependency => Edge[InOrgProject] = {
    dep =>
      new Edge[InOrgProject] {
        override def parent: InOrgProject = dep.parent

        override def child: InOrgProject = dep.child

        val label = Some(dep.scope match {
          case None => dep.child.version
          case Some(sc) => s"${dep.child.version} ($sc)"
        })
        override val style: Option[LineStyle] =
          dep.scope.map {
            case "test" => GraphViz.Dashed
            case "provided" => GraphViz.Dotted
          }
      }
  }

  def projectNode: InOrgProject => GraphViz.Node = { project =>
    new GraphViz.Node {
      override def id: NodeId = NodeId(GraphViz.dashesToUnderscores(project.name))

      override def label: String = s"${project.name} ${project.version}"
    }
  }

  val dependenciesOf: ProjectName => Seq[IntraOrgDependency] = { dep =>
    git.bringDown(dep) match {
      case Some(dir) => Maven.dependenciesFromPom(MavenGroup)(dir)
      case None => Seq()
    }
  }
  def findAllDependencies(startingProject: ProjectName): Seq[IntraOrgDependency] = {

    def go(allDeps: Map[ProjectName, Seq[IntraOrgDependency]], investigate: List[ProjectName]): Map[ProjectName, Seq[IntraOrgDependency]] = {
      if (investigate.isEmpty)
        allDeps
      else {
        val next :: rest = investigate
        if (allDeps.contains(next))
          go(allDeps, rest)
        else {
          val deps = dependenciesOf(next)
          go(allDeps + (next -> deps), rest ++ deps.map(_.child.name))
        }
      }
    }

    val allDeps = go(Map(), List(startingProject))

    allDeps.values.flatten.toSeq
  }


  val edges = findAllDependencies(StartingProject)
  val projectNodes = edges.map(_.parent).map(projectNode)
  val r = GraphViz.makeAPicture(OutputName, edges.map(dependencyEdge), projectNode, projectNodes)
  println(s"There is a picture for you in ${r.getAbsolutePath}")


  val buildOrder = Linearize.tsort(edges.map { case dep => (dep.parent.name, dep.child.name) }).toList.reverse
  println(s"Here is an order for building: ${buildOrder}")


  Seq("install" -> "mvn install", "clean" -> "mvn clean", "fetch" -> "git fetch && git merge --ff-only").foreach { case (name, command) =>
    val scriptName = s"${name}_all"
    val buildScript = BuildScript.createBuildScript(BuildFileLocation, command, scriptName, buildOrder)
    println(s"There is a script for you in $buildScript")
  }

}




object Linearize {

  // https://gist.github.com/ThiporKong/4399695
  def tsort[A](edges: Traversable[(A, A)]): Iterable[A] = {
    @tailrec
    def tsort(toPreds: Map[A, Set[A]], done: Iterable[A]): Iterable[A] = {
      val (noPreds, hasPreds) = toPreds.partition { _._2.isEmpty }
      if (noPreds.isEmpty) {
        if (hasPreds.isEmpty) done else sys.error(hasPreds.toString)
      } else {
        val found = noPreds.map { _._1 }
        tsort(hasPreds.mapValues { _ -- found }, done ++ found)
      }
    }

    val toPred = edges.foldLeft(Map[A, Set[A]]()) { (acc, e) =>
      acc + (e._1 -> acc.getOrElse(e._1, Set())) + (e._2 -> (acc.getOrElse(e._2, Set()) + e._1))
    }
    tsort(toPred, Seq())
  }
}
