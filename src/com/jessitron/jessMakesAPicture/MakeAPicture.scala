package com.jessitron.jessMakesAPicture

import java.io.File

import com.jessitron.jessMakesAPicture.git.Git
import com.jessitron.jessMakesAPicture.graphviz.GraphViz
import com.jessitron.jessMakesAPicture.graphviz.GraphViz.{Edge, LineStyle}
import com.jessitron.jessMakesAPicture.maven.Maven.{InOrgProject, IntraOrgDependency}

import scala.xml.{Node, XML}

object MakeAPicture extends App {


  val StartingProject = "rug-cli"
  val MavenGroup = "com.atomist"
  val OutputName = "atomist"

  def dependencyEdge: IntraOrgDependency => Edge[InOrgProject] = {
    dep =>
      new Edge[InOrgProject] {
        override def parent: InOrgProject = dep.parent

        override def child: InOrgProject = dep.child

        val label = Some(dep.scope match {
          case None => dep.version
          case Some(sc) => s"${dep.version} ($sc)"
        })
        override val style: Option[LineStyle] =
          dep.scope.map {
            case "test" => GraphViz.Dashed
            case "provided" => GraphViz.Dotted
          }
      }
  }


  def dependenciesFromPom(groupId: String): File => Seq[IntraOrgDependency] = { projectDir: File =>
    val pom = projectDir.listFiles().toList.find(_.getName == "pom.xml").getOrElse {
      throw new RuntimeException(s"No pom.xml found in ${projectDir.getName}")
    }
    val projectXml = XML.loadFile(pom)
    val parentName = (projectXml \ "artifactId").text
    val deps = projectXml \ "dependencies" \ "dependency"
    val atomistDeps = deps.filter(node => (node \ "groupId").text == groupId)

    def interpretDependencyNode(node: Node): IntraOrgDependency = {
      val childName = (node \ "artifactId").text
      val scope = (node \ "scope").text
      val version = (node \ "version").text
      IntraOrgDependency(parentName, childName, version, if (scope.isEmpty) scala.None else Some(scope))
    }

    atomistDeps.map(interpretDependencyNode)
  }

  val dependenciesOf: InOrgProject => Seq[IntraOrgDependency] = Git.bringDown andThen dependenciesFromPom(MavenGroup)


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
  val r = GraphViz.makeAPicture(OutputName, edges.map(dependencyEdge), GraphViz.stringToNode)
  println(s"There is a picture for you in ${r.getAbsolutePath}")

}
