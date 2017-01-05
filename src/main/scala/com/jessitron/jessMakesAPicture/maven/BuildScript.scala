package com.jessitron.jessMakesAPicture.maven

import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path, Paths}

import com.jessitron.jessMakesAPicture.maven.Maven.ProjectName


object BuildScript {

  type Command = String

  // this 'title' thing works on my terminal, dunno about yours
  val header =
    """#!/bin/bash
      |set -x
      |
      |function title {
      |    echo -ne "\033]0;"$*"\007"
      |}
      |
    """.stripMargin

  def footer(command: Command) =
    s"""
      |title "$command complete"
    """.stripMargin

  def createBuildScript(command: Command, locationString: String, projects: Seq[ProjectName]): Path = {

    val newContent = buildScriptFor(command, projects).getBytes(StandardCharsets.UTF_8)

    val location = Paths.get(locationString)
    if (Files.exists(location)) {
      val existingContent = Files.readAllBytes(location)
      if (existingContent.toString == newContent) {
        println("Build file is already there, no updates")
        return location
      }
      println("About to overwrite build file")
    }

    // this will fail if the location points to a dir that doesn't exist
    val path = Files.write(location, newContent)
    Files.setPosixFilePermissions(path, PosixFilePermissions.fromString("rwxr-x---"))
    path
  }

  private def buildScriptFor(command: Command, projects: Seq[ProjectName]) = {
    header + projects.zipWithIndex.map(indexFromOne).map(buildOne(command, projects.length)).mkString("\n") + footer(command)
  }

  private def indexFromOne[A](zipped: (A, Int)): (A, Int) =
    zipped match {
      case (a, i) => (a, i+1)
    }

  private def buildOne(command: Command, total: Int): PartialFunction[(ProjectName, Int), String] = {
    case (project: ProjectName, n: Int) =>
      s"""
         |echo "\n  Now working on: $project\n"
         |cd $project
         |title "$project ($n/$total)"
         |$command
         |
       |if [ $$? -ne 0 ]
         |then
         |  echo "Failure running '$command' in $project"
         |  exit 1
         |fi
         |cd -
     """.stripMargin
  }


}
