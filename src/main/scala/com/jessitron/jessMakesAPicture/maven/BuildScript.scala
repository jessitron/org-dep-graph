package com.jessitron.jessMakesAPicture.maven

import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path, Paths}

import com.jessitron.jessMakesAPicture.maven.Maven.{InOrgProject, IntraOrgDependency, ProjectName}


object BuildScript {

  type Command = String

  // this 'title' thing works on my terminal, dunno about yours
  val header =
    """#!/bin/bash
      |#  set -x
      |
      |function title {
      |    echo -ne "\033]0;"$*"\007"
      |}
      |
      |if [ $1 == "from" ]
      |then
      |   starting_project=$2
      |   started=false
      |else
      |   started=true
      |fi
    """.stripMargin

  def footer(command: Command, projects: Seq[ProjectName]) =
    s"""
      |title "$command complete"
      |
      |echo "Finished running '$command' on ${projects.mkString(",")}"
    """.stripMargin


  def createBuildScript(locationString: String, command: Command, scriptName: String, projects: Seq[ProjectName]): Path = {

    putInFile(s"$locationString/$scriptName", buildScriptForProjects(command, projects).getBytes(StandardCharsets.UTF_8))

  }

  private def putInFile(locationString: String, newContent: Array[Byte]): Path = {
    val location = Paths.get(locationString)
    if (Files.exists(location)) {
      val existingContent = Files.readAllBytes(location)
      if (existingContent.toString == new String(newContent)) {
        println(s"Build file is already in $locationString, no updates")
        return location
      }
      // println("About to overwrite build file")
    }

    // this will fail if the location points to a dir that doesn't exist
    val path = Files.write(location, newContent)
    Files.setPosixFilePermissions(path, PosixFilePermissions.fromString("rwxr-x---"))
    path
  }

  private def buildScriptForProjects(command: Command, projects: Seq[ProjectName]) =
    buildScriptFor[ProjectName]({ _ => command}, projects, command, identity)

  private def buildScriptFor[A](commandFunction: A => Command, projects: Seq[A], commandDescription: String, projectName: A => ProjectName) = {
    header + projects.zipWithIndex.map(indexFromOne).map(buildOne(projectName, commandFunction, projects.length)).mkString("\n") + footer(commandDescription, projects.map(projectName))
  }

  private def indexFromOne[A](zipped: (A, Int)): (A, Int) =
    zipped match {
      case (a, i) => (a, i+1)
    }

  private def buildOne[A](projectNameFunction: A => ProjectName, commandFunction: A => Command, total: Int): PartialFunction[(A, Int), String] = {
    case (a: A, n: Int) =>
      val project = projectNameFunction(a)
      val command = commandFunction(a)
      s"""
         |if [[ "$$started" == "true" || "$$starting_project" == "$project" ]]
         |then
         |  started=true
         |  echo "\n  Now working on: $project\n"
         |  cd $project
         |  title "$project ($n/$total)"
         |  ${command}
         |
         |  if [ $$? -ne 0 ]
         |  then
         |    echo "Failure running '$command' in $project"
         |    exit 1
         |  fi
         |  cd -
         |fi
     """.stripMargin
  }

  def createScriptToLinkLocalVersions(locationString: String, group: Maven.Group, deps: Seq[IntraOrgDependency], localProjects: Seq[InOrgProject]): Path = {

    val localVersionsByName = localProjects.map { case InOrgProject(name, version) => (name, version) }.toMap

    val commandFromDep: IntraOrgDependency => Command = { dep =>
      localVersionsByName.get(dep.child.name) match {
        case Some(localVersion) =>
          s"rug edit -R atomist-rugs:common-editors:ChangeDependencyVersion group_id=$group artifact_id=${dep.child.name} new_version=${localVersion}"
        case None =>
          s"echo no local version of ${dep.child.name}"
      }
    }

    val description: IntraOrgDependency => ProjectName = {
      iod => iod.parent.name
    }

    BuildScript.putInFile(s"${locationString}/depend_on_local_versions", buildScriptFor(commandFromDep, deps, "use local dependencies", description).getBytes(StandardCharsets.UTF_8))
  }

}
