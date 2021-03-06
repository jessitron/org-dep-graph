package com.jessitron.jessMakesAPicture

import com.jessitron.jessMakesAPicture.graphviz.GraphViz
import com.jessitron.jessMakesAPicture.graphviz.GraphViz._
import com.jessitron.jessMakesAPicture.maven.Maven.{InOrgProject, IntraOrgDependency}

object GraphVizInterop {
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

  def projectNode: CombinedProjectData => GraphViz.Node = { project =>

    project match {
      case UnfoundProject(name) =>
        new GraphViz.Node {
          override def id: NodeId = NodeId(project.name)

          override def label = s"$name (not found!)"
        }
      case FoundProject(iop, repo) =>
        new GraphViz.Node {
          override def id: NodeId = NodeId(project.name)

          override def label: String = s"${project.name} ${iop.version}" + (if (repo.currentBranch != "master") s"\n${repo.currentBranch}" else "")

          override def color: NodeColor = if (repo.dirty) LightBlue else White
        }
    }
  }

  def projectNodeId: InOrgProject => GraphViz.NodeId = { p =>
    NodeId(p.name)
  }

}
