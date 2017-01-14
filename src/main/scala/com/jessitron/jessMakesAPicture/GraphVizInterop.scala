package com.jessitron.jessMakesAPicture

import com.jessitron.jessMakesAPicture.graphviz.GraphViz
import com.jessitron.jessMakesAPicture.graphviz.GraphViz.{Edge, LineStyle, NodeId}
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

  def projectNode: InOrgProject => GraphViz.Node = { project =>
    new GraphViz.Node {
      override def id: NodeId = NodeId(GraphViz.dashesToUnderscores(project.name))

      override def label: String = s"${project.name} ${project.version}"
    }
  }


}
