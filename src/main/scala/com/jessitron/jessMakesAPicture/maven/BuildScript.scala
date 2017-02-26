package com.jessitron.jessMakesAPicture.maven

import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path, Paths}

import com.jessitron.jessMakesAPicture.maven.Maven.{InOrgProject, IntraOrgDependency, ProjectName, Version}


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
    """.stripMargin

  def footer(name: String, projects: Seq[ProjectName]) =
    s"""
      |title "$name complete"
      |
      |echo "Finished running '$name' on ${projects.mkString(",")}"
    """.stripMargin


  def createBuildScript(locationString: String, command: Command, scriptName: String, projects: Seq[ProjectName]): Path = {

    putInFile(s"$locationString/$scriptName", buildScriptForProjects(scriptName, command, projects).getBytes(StandardCharsets.UTF_8))

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

  private def buildScriptForProjects(scriptName: String, command: Command, projects: Seq[ProjectName]) =
    buildScriptFor[ProjectName](scriptName, { _ => command}, projects, command, identity)

  private def buildScriptFor[A](scriptName: String, commandFunction: A => Command, projects: Seq[A], commandDescription: String, projectName: A => ProjectName) = {
    header + projects.zipWithIndex.map(indexFromOne).map(buildOne(projectName, commandFunction, projects.length)).mkString("\n") + footer(scriptName, projects.map(projectName))
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
         |echo "\n  Now working on: $project\n"
         |cd $project
         |title "$project ($n/$total)"
         |${command}
         |
         |if [ $$? -ne 0 ]
         |then
         |  echo "Failure running '$command' in $project"
         |  exit 1
         |fi
         |cd -
     """.stripMargin
  }

  def createScriptToLinkLocalVersions(locationString: String,
                                      group: Maven.Group,
                                      deps: Seq[(String, String)], // (parent, child)
                                      localProjects: Seq[InOrgProject],
                                      baseProject: InOrgProject): Path = {

    val localVersionsByName = localProjects.map { case InOrgProject(name, version) => (name, version) }.toMap

    val versionWithoutSnapshot = baseProject.version.replace("-SNAPSHOT", "")
    val desiredBranch = s"${baseProject.name}-$versionWithoutSnapshot"


    val commandFromDep: ((String,String)) => Command = { case (parent, child) =>
      localVersionsByName.get(child) match {
        case Some(localVersion) =>
          checkoutBranch(desiredBranch) + updateDependency(group, child, localVersion)
        case None =>
          s"echo no local version of ${child}"
      }
    }


    val description: IntraOrgDependency => ProjectName = {
      iod => iod.parent.name
    }

    BuildScript.putInFile(s"$locationString/depend_on_local_versions", buildScriptFor[(String,String)](locationString, commandFromDep, deps, "use local dependencies", _._1).getBytes(StandardCharsets.UTF_8))
  }

  private def checkoutBranch(desiredBranch: String): String = {

    val currentBranch = "$(git rev-parse --abbrev-ref HEAD"

    def exists(branch: String) = s"git rev-parse --verify $branch"

    s"""if [[ "$currentBranch" != "$desiredBranch" ]] ; then
                               |   # We need to change branches.
                               |   # Does the desired branch exist?
                               |   if ${exists(desiredBranch)} ; then
                               |      git checkout $desiredBranch || echo "Unable to check out $desiredBranch" && exit 1
                               |   else
                               |      # Does the desired branch exist on origin?
                               |      if ${exists("origin/" + desiredBranch)} ; then
                               |        git checkout $desiredBranch || echo "Unable to check out $desiredBranch" && exit 1
                               |      else
                               |      # create the branch
                               |        git checkout -b $desiredBranch || echo "Unable to create branch $desiredBranch && exit 1
                               |      fi
                               |   fi
                               |fi
                               |""".stripMargin

  }

  private def updateDependency(group: Maven.Group, childProject: Maven.ProjectName, localVersion: Version): String =
    s"\nrug edit -R atomist-rugs:common-editors:ChangeDependencyVersion group_id=$group artifact_id=${childProject} new_version=${localVersion}"


}
