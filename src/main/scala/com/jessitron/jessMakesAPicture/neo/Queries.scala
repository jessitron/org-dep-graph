package com.jessitron.jessMakesAPicture.neo

import com.jessitron.jessMakesAPicture.maven.Scope
import com.jessitron.jessMakesAPicture.neo.Neo4J.{DependencyRelationship, ProjectNode, ProjectNodeIdentifyingFactor}
import org.neo4j.driver.v1.types.{Path, Relationship}

import scala.collection.JavaConverters._

object Queries {

  def findConnectingEdges(from: ProjectNodeIdentifyingFactor, to: ProjectNodeIdentifyingFactor): Seq[DependencyRelationship] = {
    val statement = s"""match path = (a: Project {name:"${from}"})-[r*]->(b: Project {name:"${to}"}) return path"""

    val result = Connectivity.runCypherForResults(statement)

    val allPaths: Seq[Path] = result.map(_.get("path")).map(_.asPath)

    val nodes: Map[Long, ProjectNodeIdentifyingFactor] =
      allPaths.flatMap(_.nodes().asScala).map(n => (n.id(), n.get("name").asString)).toMap

    val relationships = allPaths.flatMap(_.relationships().asScala).map(relationshipToDependency(nodes))

    relationships
  }

  private def relationshipToDependency(nodes: Map[Long, ProjectNodeIdentifyingFactor])(r: Relationship): DependencyRelationship =
    DependencyRelationship(nodes.get(r.startNodeId()).get,
      nodes.get(r.endNodeId()).get,
      r.get("version").asString,
      Scope.fromString(r.get("scope").asString))


}

object QueryPlay extends App {

  val result = Queries.findConnectingEdges("rug-cli", "rug")

  println(result)

}
