package com.jessitron.jessMakesAPicture.neo

import org.neo4j.driver.v1._


object Connectivity {

  val driver: Driver = GraphDatabase.driver("bolt://localhost:7687", AuthTokens.basic("neo4j", "neo4j"))
  val session: Session = driver.session()

  session.run("CREATE (a:Person {name: {name}, title: {title}})",
    Values.parameters("name", "Arthur", "title", "King"))

  val result: StatementResult = session.run("MATCH (a:Person) WHERE a.name = {name} " +
    "RETURN a.name AS name, a.title AS title",
    Values.parameters("name", "Arthur"))
  while (result.hasNext) {
    val record : Record = result.next()
    System.out.println(record.get("title").asString() + " " + record.get("name").asString())
  }

  session.close()
  driver.close()

}
