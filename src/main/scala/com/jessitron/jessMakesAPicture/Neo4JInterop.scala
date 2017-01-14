package com.jessitron.jessMakesAPicture

import com.jessitron.jessMakesAPicture.neo.Neo4J.{DependencyRelationship, ProjectNode}
import com.jessitron.jessMakesAPicture.maven.{Compile, Provided, Test}
import com.jessitron.jessMakesAPicture.maven.Maven.{InOrgProject, IntraOrgDependency}
import com.jessitron.jessMakesAPicture.neo.Neo4J

object Neo4JInterop {

  def projectNode(maven: InOrgProject): Neo4J.ProjectNode = {
    ProjectNode(name = maven.name, version = maven.version)
  }

  def dependencyEdge(dep: IntraOrgDependency):  Neo4J.DependencyRelationship = {
    // todo: move this to maven
    val scope = dep.scope match {
      case None => Compile
      case Some("provided") => Provided
      case Some("test") => Test
      case other => throw new RuntimeException(s"Please add support for ${other} scope")
    }
    DependencyRelationship(projectNode(dep.parent), projectNode(dep.child), scope)
  }

}
