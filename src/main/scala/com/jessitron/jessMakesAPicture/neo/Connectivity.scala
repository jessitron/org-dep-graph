package com.jessitron.jessMakesAPicture.neo

import org.neo4j.driver.v1._


object Connectivity {

  def runCypher(statements:Seq[String]) = {

    val driver: Driver = GraphDatabase.driver("bolt://localhost:7687", AuthTokens.basic("neo4j", "jessitron"))
    val session: Session = driver.session()

    for (statement <- statements) {
      val result: StatementResult = session.run(statement,
        Values.parameters())
    }

    session.close()
    driver.close()
  }

}
