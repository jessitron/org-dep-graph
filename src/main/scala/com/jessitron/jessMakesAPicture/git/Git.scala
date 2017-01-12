package com.jessitron.jessMakesAPicture.git

import java.io.File

class GitHubOrg(url: String) {

  import sys.process._

  val FetchForUptodateness = true

  private def fetchIn(dir: File): File = {
    println(s"fetching in ${dir.getName}")
    val fetchExitCode = Seq("git", "-C", dir.getPath, "fetch").!
    if (fetchExitCode != 0) throw new RuntimeException(s"Could not fetch in ${dir.getPath}")
    val ffExitCode = Seq("git", "-C", dir.getPath, "merge", "--ff-only").!
    if (ffExitCode != 0) {
      println(s"WARNING: could not bring repository in ${dir.getPath} up to date.")
    }
    dir
  }

  /**
    * Clone a repo in the current directory
    *
    * @return the cloned directory as a File
    */
  def bringDown: String => Option[File] = { project =>
    val existingDir = new File(project)
    if (existingDir.isDirectory) {
      if (FetchForUptodateness)
        Some(fetchIn(existingDir))
      else
        Some(existingDir)
    }
    else {
      val gitUrl = s"$url$project"
      val exitCode = Seq("git", "clone", gitUrl).!
      if (exitCode != 0) {
        println(s"git clone $gitUrl failed with $exitCode")
        println(s"Skipping $project")
        None
      } else {
        val dir = new File(project)
        if (!dir.isDirectory) throw new RuntimeException(s"git clone $gitUrl did not produce directory $project")
        Some(dir)
      }
    }
  }

}
