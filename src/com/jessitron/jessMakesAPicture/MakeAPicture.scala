package com.jessitron.jessMakesAPicture

import com.jessitron.jessMakesAPicture.git.GitHubOrg
import com.jessitron.jessMakesAPicture.graphviz.GraphViz
import com.jessitron.jessMakesAPicture.graphviz.GraphViz.{Edge, LineStyle, NodeId}
import com.jessitron.jessMakesAPicture.maven.Maven
import com.jessitron.jessMakesAPicture.maven.Maven.{InOrgProject, IntraOrgDependency, ProjectName}

object MakeAPicture extends App {

  val StartingProject = "rug-cli"
  val GitHubUrl = "git@github.com-personal:atomist/"
  val MavenGroup = "com.atomist"
  val OutputName = "atomist"

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
        if(allDeps.contains(next))
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

}
