package com.jessitron.jessMakesAPicture

import com.jessitron.jessMakesAPicture.git.GitHubOrg
import com.jessitron.jessMakesAPicture.graphviz.GraphViz
import com.jessitron.jessMakesAPicture.maven.{BuildScript, Maven}
import com.jessitron.jessMakesAPicture.maven.Maven.{InOrgProject, IntraOrgDependency, ProjectName}

import scala.annotation.tailrec

object MakeAPicture extends App {

  val StartingProject = "rug-cli"
  val GitHubUrl = "git@github.com:atomist/"
  val MavenGroup = "com.atomist"
  val OutputName = "atomist"
  val BuildFileLocation = "bin/"

  val git = new GitHubOrg(GitHubUrl, fetch = false)

  val runId = System.currentTimeMillis().toString


  private val investigateProject: ProjectName => (InOrgProject, Seq[IntraOrgDependency]) = { dep =>
    git.bringDown(dep) match {
      case Some(dir) => Maven.analyzePom(MavenGroup)(dir)
      case None => (InOrgProject(dep, "not present"), Seq())
    }
  }
  def findAllDependencies(startingProject: ProjectName): (Seq[InOrgProject], Seq[IntraOrgDependency]) = {

    def go(allDeps: Map[ProjectName, Seq[IntraOrgDependency]], allProjects: Seq[InOrgProject], investigate: List[ProjectName])
    : (Seq[InOrgProject], Map[ProjectName, Seq[IntraOrgDependency]]) = {
      if (investigate.isEmpty)
        (allProjects, allDeps)
      else {
        val next :: rest = investigate
        if (allDeps.contains(next))
          go(allDeps, allProjects, rest)
        else {
          val (project, deps) = investigateProject(next)
          go(allDeps + (next -> deps), allProjects :+ project, rest ++ deps.map(_.child.name))
        }
      }
    }

    val (allProjects, allDeps) = go(Map(), Seq(), List(startingProject))

    (allProjects, allDeps.values.flatten.toSeq)
  }


  val (projects, edges) = findAllDependencies(StartingProject)
  val r = GraphViz.makeAPicture(OutputName, edges.map(GraphVizInterop.dependencyEdge), GraphVizInterop.projectNode, projects.map(GraphVizInterop.projectNode))
  println(s"There is a picture for you in ${r.getAbsolutePath}")

  val n = Neo4J.makeAPicture(edges.map(Neo4JInterop.dependencyEdge), projects.map(Neo4JInterop.projectNode), runId)


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
