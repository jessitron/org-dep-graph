package com.jessitron.jessMakesAPicture.git

import java.io.File


object GitHub {
  type Org = String
  type Repo = String

  type Branch = String

  type HowFar = Int
}

case class GitRepo(contents: File, currentBranch: GitHub.Branch, behind: GitHub.HowFar, ahead: GitHub.HowFar, dirty: Boolean)

object GitRepo {
  def justCloned(contents:File) = GitRepo(contents, "master", 0, 0, false)
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
  def bringDown: String => Option[GitRepo] = { project =>
    val existingDir = new File(project)
    if (existingDir.isDirectory) {
      if (fetch)
        Some(CheckGitStatus.checkStatus(fetchIn(existingDir)))
      else
        Some(CheckGitStatus.checkStatus(existingDir))
    }
    else {
      clone(project, orgs).map(GitRepo.justCloned)
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

object CheckGitStatus {

  import sys.process._
  def checkStatus(projectDir: File) = {
    val statusProcess = Seq("git", "-C" , projectDir.getPath, "status", "--porcelain", "--branch").lineStream
    /* ## banana...origin/banana [ahead 1, behind 1]
       A  file-in-index
       ?? untracked-file
     */
    val branchLine = statusProcess.head
    val changes = statusProcess.length - 1
    if (branchLine == "## HEAD (no branch)") {
      GitRepo(projectDir, "HEAD", 0, 0, changes > 0)
    } else {
      val branch = branchLine.substring(3).split("\\.").head
      val ahead: Int = if (branchLine.contains("ahead")) branchLine.split("ahead ")(1).split("[,\\]]").head.toInt else 0
      val behind: Int = if (branchLine.contains("behind")) branchLine.split("behind ")(1).split("\\]").head.toInt else 0
      GitRepo(projectDir, branch, ahead, behind, changes > 0)
    }

  }

  def main(args: Array[String]): Unit = {
   val result =  checkStatus(new File("/tmp/foo2"))
    println(result)
  }
}
