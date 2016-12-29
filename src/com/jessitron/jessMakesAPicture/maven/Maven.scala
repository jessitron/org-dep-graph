package com.jessitron.jessMakesAPicture.maven

import java.io.File

import scala.xml.{Elem, Node, XML}

object Maven {

  type InOrgProject = String //GOAL: case class InOrgProject(name: String, version: Version)
  type Version = String

  case class IntraOrgDependency(parent: InOrgProject, child: InOrgProject, version: Version, scope: Option[String])

  def dependenciesFromPom(groupId: String): File => Seq[IntraOrgDependency] = { projectDir: File =>
    val projectXml = pomXml(projectDir)
    val parentName = (projectXml \ "artifactId").text
    val deps = projectXml \ "dependencies" \ "dependency"
    val intraOrgDeps = deps.filter(node => (node \ "groupId").text == groupId)

    def interpretDependencyNode(node: Node): IntraOrgDependency = {
      val childName = (node \ "artifactId").text
      val scope = (node \ "scope").text
      val version = (node \ "version").text
      IntraOrgDependency(parentName, childName, version, if (scope.isEmpty) scala.None else Some(scope))
    }

    intraOrgDeps.map(interpretDependencyNode)
  }

  private def pomXml(projectDir: File): Elem = {
    val pom = projectDir.listFiles().toList.find(_.getName == "pom.xml").getOrElse {
      throw new RuntimeException(s"No pom.xml found in ${projectDir.getName}")
    }
    XML.loadFile(pom)
  }


}
