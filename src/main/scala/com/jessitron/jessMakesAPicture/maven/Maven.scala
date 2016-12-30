package com.jessitron.jessMakesAPicture.maven

import java.io.File

import scala.xml.{Elem, Node, XML}

object Maven {

  type ProjectName = String
  type Version = String
  case class InOrgProject(name: ProjectName, version: Version)

  case class IntraOrgDependency(parent: InOrgProject, child: InOrgProject, scope: Option[String])

  def dependenciesFromPom(groupId: String): File => Seq[IntraOrgDependency] = { projectDir: File =>
    val projectXml = pomXML(projectDir)
    val parentVersion = projectVersion(projectXml)
    val parentName = (projectXml \ "artifactId").text
    val parent = InOrgProject(parentName, parentVersion)

    val properties = projectProperties(projectXml)


    val deps = projectXml \ "dependencies" \ "dependency"
    val intraOrgDeps = deps.filter(node => (node \ "groupId").text == groupId)

    def interpretDependencyNode(node: Node): IntraOrgDependency = {
      val childVersion = substituteProperty(properties, (node \ "version").text)
      val child = InOrgProject((node \ "artifactId").text, childVersion)
      val scope = (node \ "scope").text
      IntraOrgDependency(parent, child, if (scope.isEmpty) scala.None else Some(scope))
    }

    intraOrgDeps.map(interpretDependencyNode)
  }

  private def projectProperties(pomXML: Elem): Map[String, String] = {
    val propertyElements = (pomXML \ "properties").collect { case e: Elem => e.child }.flatten
    val o = propertyElements.collect({ case e: Elem => (e.label, e.text)}).toMap
    o
  }

  private def substituteProperty(props: Map[String,String], in: String): String = {
    var str = in
    props.foreach {
      case (key, value) =>
        str = str.replace("${" + key + "}", value)
    }
    str
  }

  private def pomXML(projectDir: File): Elem = {
    val pom = projectDir.listFiles().toList.find(_.getName == "pom.xml").getOrElse {
      throw new RuntimeException(s"No pom.xml found in ${projectDir.getName}")
    }
    XML.loadFile(pom)
  }

  private def projectVersion(pomXML: Elem): Version = {
    (pomXML \ "version").text
  }


}
