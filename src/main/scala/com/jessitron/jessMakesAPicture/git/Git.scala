package com.jessitron.jessMakesAPicture.git

import java.io.File


object GitHub {
  type Org = String
  type Repo = String
}

class GitHubOrg(orgs: Seq[GitHub.Org], fetch: Boolean = true) {

  import sys.process._


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
      if (fetch)
        Some(fetchIn(existingDir))
      else
        Some(existingDir)
    }
    else {
      clone(project, orgs)
    }
  }

  private def clone(project: GitHub.Repo, possibleOrgs: Seq[GitHub.Org] ): Option[File] = {
    var successful = false

    val urls = orgs.map { org => s"git@github.com:$org/$project" }

    for (url <- urls if !successful) {
      val exitCode = Seq("git", "clone", url).!
      if (exitCode == 0)
        successful = true
    }
    if (successful) {
      val dir = new File(project)
      if (!dir.isDirectory) throw new RuntimeException(s"git clone did not produce directory $project")
      Some(dir)
    } else {
      println(s"git clone failed at all of ${urls.mkString(" ")}")
      println(s"Skipping $project")
      None
    }
  }

}
