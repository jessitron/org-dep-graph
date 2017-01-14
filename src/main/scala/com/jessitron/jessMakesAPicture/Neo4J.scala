package com.jessitron.jessMakesAPicture

import com.jessitron.jessMakesAPicture.maven.Maven.{ProjectName, Version}
import com.jessitron.jessMakesAPicture.maven.Scope

object Neo4J {

  type UniqueId = String
  type Run = String

  case class ProjectNode(name: ProjectName, version: Version) {

    def createSyntax(runId: Run, projectIds: Map[ProjectName, UniqueId])  : String =
     s"""(${projectIds(name)} { name: "$name", version: "$version", asOf: "$runId" })"""
  }

  case class DependencyRelationship(parent: ProjectNode, child: ProjectNode, scope: Scope) {
    def createSyntax(projectIds: Map[ProjectName, UniqueId]) : String =
      s"""(${projectIds(parent.name)})-[:DEPENDS_ON]->(${projectIds(child.name)})"""
  }



  /*
   * now, if the relationships contained two nodes, I wouldn't have to pass the nodes separately.
   * But in this domain, the nodes represent the projects we have, while the depended-on versions aren't
   * necessarily among them.
   * Right this second I'm connecting them on projectName only. In the future I'd like to represent different
   * versions as different nodes because let's face it, they're different programs.
   * Then, any project-versions that are in my computer but not enmeshed in the dependency web will be the extra nodes.
   */
  def makeAPicture(relationships: Seq[DependencyRelationship], drawNodes: Seq[ProjectNode], runId: Run): Unit = {

    val uniqueNames = Stream.from(1).map("p" + _).iterator
    val nodeIdByProjectName = drawNodes.map(pn => (pn.name, uniqueNames.next())).toMap

    val cypher = "CREATE " +
      (drawNodes.map(_.createSyntax(runId, nodeIdByProjectName)) ++ relationships.map(_.createSyntax(nodeIdByProjectName))).mkString(",\n")

    println(s"\n$cypher\n")
  }

}
