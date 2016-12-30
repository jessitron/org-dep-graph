package com.jessitron.jessMakesAPicture

import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path, Paths}

import com.jessitron.jessMakesAPicture.git.GitHubOrg
import com.jessitron.jessMakesAPicture.graphviz.GraphViz
import com.jessitron.jessMakesAPicture.graphviz.GraphViz.{Edge, LineStyle, NodeId}
import com.jessitron.jessMakesAPicture.maven.Maven
import com.jessitron.jessMakesAPicture.maven.Maven.{InOrgProject, IntraOrgDependency, ProjectName}

import scala.annotation.tailrec

object MakeAPicture extends App {

  val StartingProject = "rug-cli"
  val GitHubUrl = "git@github.com:atomist/"
  val MavenGroup = "com.atomist"
  val OutputName = "atomist"
  val BuildFileLocation = "bin/build.sh"

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


  val dependenciesOf: ProjectName => Seq[IntraOrgDependency] = git.bringDown andThen Maven.dependenciesFromPom(MavenGroup)

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

  val buildScript = BuildScript.createBuildScript(BuildFileLocation, buildOrder)

}

object BuildScript {

  val header=
    """#!/bin/bash
      |set -e
      |set -x
      |
    """.stripMargin

  def createBuildScript(locationString: String, projects: Seq[ProjectName]): Path = {

    val newContent = buildScriptFor(projects).getBytes(StandardCharsets.UTF_8)

    val location = Paths.get(locationString)
    if (Files.exists(location)) {
      val existingContent = Files.readAllBytes(location)
      if (existingContent.toString == newContent) {
        println("Build file is already there, no updates")
        return location
      }
      println("About to overwrite build file")
    }

    // this will fail if the location points to a dir that doesn't exist
    val path = Files.write(location, newContent)
    Files.setPosixFilePermissions(path, PosixFilePermissions.fromString("rwxr-x---"))
    path
  }

  def buildScriptFor(projects: Seq[ProjectName]) = {
    header + projects.map(buildOne).mkString("\n")
  }

  private def buildOne(project: ProjectName) = {
    s"""
       |echo "\n  Building $project\n"
       |cd $project
       |mvn install
       |cd -
     """.stripMargin
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
