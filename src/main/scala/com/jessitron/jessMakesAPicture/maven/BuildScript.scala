package com.jessitron.jessMakesAPicture.maven

import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path, Paths}

import com.jessitron.jessMakesAPicture.maven.Maven.ProjectName

object BuildScript {

  val header=
    """#!/bin/bash
      |set -e
      |set -x
      |
    """.stripMargin

  def createBuildScript(locationString: String, projects: Seq[ProjectName]): Path = {

    val newContent = buildScriptFor(projects).getBytes(StandardCharsets.UTF_8)

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

  def buildScriptFor(projects: Seq[ProjectName]) = {
    header + projects.map(buildOne).mkString("\n")
  }

  private def buildOne(project: ProjectName) = {
    s"""
       |echo "\n  Building $project\n"
       |cd $project
       |mvn install
       |cd -
     """.stripMargin
  }


}
