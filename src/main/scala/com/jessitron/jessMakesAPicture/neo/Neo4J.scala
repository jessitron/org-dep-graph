package com.jessitron.jessMakesAPicture.neo

import java.util.Date

import com.jessitron.jessMakesAPicture.git.GitHub
import com.jessitron.jessMakesAPicture.maven.Maven.{ProjectName, Version}
import com.jessitron.jessMakesAPicture.maven.Scope

object Neo4J {

  type UniqueId = String
  type Run = String

  case class ProjectNode(name: ProjectName, version: Version, branch: GitHub.Branch) {

    def now = new Date().toString

    def createSyntax(runId: Run, projectIds: Map[ProjectName, UniqueId])  : String =
     s"""(${projectIds(name)}:Project { name: "$name", version: "$version", branch: "$branch", asOf: "$runId", created: "$now" } )"""
  }

  type ProjectNodeIdentifyingFactor = ProjectName

  case class DependencyRelationship(parent: ProjectNodeIdentifyingFactor, child: ProjectNodeIdentifyingFactor, childVersion: Version,  scope: Scope) {
    def createSyntax(projectIds: Map[ProjectNodeIdentifyingFactor, UniqueId]) : String =
      s"""(${projectIds(parent)})-[:DEPENDS_ON { scope: "${scope.toString().toLowerCase()}", version:"${childVersion}" }]->(${projectIds(child)})"""
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

    try {
      Connectivity.runCypher(Seq(cypher, deleteOthers(runId)))
    } catch {
      case e: Exception =>
         println(s"Unable to run the cypher because: ${e.getMessage}")
         println(s"Here is the cypher I would have run:\n\n$cypher\n")
    }

    println(s"See the relationships at http://localhost:7474 with:\n\n${matchQuery(runId)}\n")
  }

  def matchQuery(run: Run):String = {
    s"""MATCH (p: Project)
       |   WHERE p.asOf = "$run"
       |RETURN p""".stripMargin
  }

  def deleteOthers(run: Run): String = {
    s"""MATCH (a:Project)
       |    WHERE a.asOf <> "${run}"
       |OPTIONAL MATCH (a)-[r1]-()
       |DELETE a, r1""".stripMargin
  }

}
