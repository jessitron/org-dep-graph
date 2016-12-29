package com.jessitron.jessMakesAPicture.git

import java.io.File

object Git {

  import sys.process._

  val GitHubUrl = "git@github.com-personal:atomist/"
  val FetchForUptodateness = false

  private def fetchIn(dir: File): File = {
    println(s"fetching in ${dir.getName}")
    val fetchExitCode = Seq("git", "-C", dir.getPath, "fetch").!
    if (fetchExitCode != 0) throw new RuntimeException(s"Could not fetch in ${dir.getPath}")
    dir
  }

  /**
    * Clone a repo in the current directory
    *
    * @return the cloned directory as a File
    */
  def bringDown: String => File = { project =>
    val existingDir = new File(project)
    if (existingDir.isDirectory) {
      if (FetchForUptodateness)
        fetchIn(existingDir)
      else
        existingDir
    }
    else {
      val gitUrl = s"$GitHubUrl$project"
      val exitCode = Seq("git", "clone", gitUrl).!
      if (exitCode != 0) throw new RuntimeException(s"git clone $gitUrl failed with $exitCode")
      val dir = new File(project)
      if (!dir.isDirectory) throw new RuntimeException(s"git clone $gitUrl did not produce directory $project")
      dir
    }
  }

}
