package com.jessitron.jessMakesAPicture

import com.jessitron.jessMakesAPicture.neo.Neo4J.{DependencyRelationship, ProjectNode}
import com.jessitron.jessMakesAPicture.maven.{Compile, Provided, Test}
import com.jessitron.jessMakesAPicture.maven.Maven.{IntraOrgDependency}
import com.jessitron.jessMakesAPicture.neo.Neo4J

object Neo4JInterop {

  def projectNode(p: CombinedProjectData): Neo4J.ProjectNode = {
    p match {
      case UnfoundProject(n) => ProjectNode(name = n, version = "not found!", branch = "not found!")
      case FoundProject(maven, git) => ProjectNode(name = maven.name, version = maven.version, branch = git.currentBranch)
    }
  }

  def dependencyEdge(dep: IntraOrgDependency):  Neo4J.DependencyRelationship = {
    // todo: move this to maven
    val scope = dep.scope match {
      case None => Compile
      case Some("provided") => Provided
      case Some("test") => Test
      case other => throw new RuntimeException(s"Please add support for ${other} scope")
    }
    DependencyRelationship(dep.parent.name, dep.child.name, dep.child.version, scope)
  }

}
