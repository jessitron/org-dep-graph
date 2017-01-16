package com.jessitron.jessMakesAPicture

import com.jessitron.jessMakesAPicture.git.GitRepo
import com.jessitron.jessMakesAPicture.maven.Maven.InOrgProject

sealed trait CombinedProjectData {
  def name: String
}
case class UnfoundProject(name: String) extends CombinedProjectData
case class FoundProject(maven: InOrgProject, git: GitRepo) extends CombinedProjectData {
  def name = maven.name
}
