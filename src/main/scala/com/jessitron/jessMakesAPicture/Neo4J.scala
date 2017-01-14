package com.jessitron.jessMakesAPicture

import com.jessitron.jessMakesAPicture.maven.Maven.{ProjectName, Version}
import com.jessitron.jessMakesAPicture.maven.Scope

object Neo4J {

  type UniqueId = String
  type Run = String

  case class ProjectNode(name: ProjectName, version: Version) {

    def createSyntax(runId: Run, projectIds: Map[ProjectName, UniqueId])  : String =
     s"""(${projectIds(name)}:Project { name: "$name", version: "$version", asOf: "$runId" })"""
  }

  case class DependencyRelationship(parent: ProjectNode, child: ProjectNode, scope: Scope) {
    def createSyntax(projectIds: Map[ProjectName, UniqueId]) : String =
      s"""(${projectIds(parent.name)})-[:DEPENDS_ON { scope: "${scope.toString().toLowerCase()}", version:"${child.version}" }]->(${projectIds(child.name)})"""
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

    println(s"This will delete all Project nodes not created by this run:\n\n${deleteOthers(runId)}\n")

    println(s"\n$cypher\n")
  }

  def deleteOthers(run: Run): String = {
    s"""MATCH (a:Project), (b:Project)
       |    WHERE a.asOf <> "${run}" AND b.asOf <> "${run}"
       |OPTIONAL MATCH (a)-[r1]-(), (b)-[r2]-()
       |DELETE a, b, r1, r2""".stripMargin
  }

}
