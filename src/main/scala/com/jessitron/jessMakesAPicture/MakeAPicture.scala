package com.jessitron.jessMakesAPicture

import com.jessitron.jessMakesAPicture.git.{GitHubOrg, GitRepo}
import com.jessitron.jessMakesAPicture.graphviz.GraphViz
import com.jessitron.jessMakesAPicture.maven.{BuildScript, Maven}
import com.jessitron.jessMakesAPicture.maven.Maven.{InOrgProject, IntraOrgDependency, ProjectName}
import com.jessitron.jessMakesAPicture.neo.Neo4J

import scala.annotation.tailrec
import scala.util.control.NonFatal

object MakeAPicture extends App {

  /* yes this is nonstandard sorry not sorry */
  val Usage = "$0 [--fetch] [base=<bottom-level-project] <top-level project...>"

  def processArgs(): (Seq[String], Boolean, Option[String]) = {
    var fetch = false
    var projects: Seq[String] = Seq()
    var baseProject: Option[String] = None
    for (arg <- args) {
      arg match {
        case "--fetch" => fetch = true
        case base if base.startsWith("base=") => baseProject = Some(base.substring(5))
        case other => projects = projects :+ other
      }
    }

    if (projects.size < 1) {
      System.err.println("No top-level project supplied")
    }

    (projects, fetch, baseProject)
  }


  val (startingProjects, fetch, baseProject) = processArgs()
  val GitHubOrgs = Seq("atomist", "atomisthq")
  val MavenGroup = "com.atomist"
  val OutputName = "atomist"
  val BuildFileLocation = "bin/"

  val git = new GitHubOrg(GitHubOrgs, fetch = fetch)

  val runId = System.currentTimeMillis().toString


  private val investigateProject: ProjectName => (CombinedProjectData, Seq[IntraOrgDependency]) = { dep =>
    val repo = git.bringDown(dep)
    repo match {
      case Some(r) =>
        try {
          val (iop, deps) = Maven.analyzePom(MavenGroup)(r.contents)
          (FoundProject(iop, r), deps)
        }
        catch {
          case NonFatal(e) => println(s"ERROR: can't analyze $dep for dependencies. ${e.getMessage}")
            (FoundProject(Maven.unparsableProject(dep), r), Seq())
        }
      case None => (UnfoundProject(dep), Seq())
    }
  }

  def findAllDependencies(startingProjects: Seq[ProjectName]): (Seq[CombinedProjectData], Seq[IntraOrgDependency]) = {

    def go(allDeps: Map[ProjectName, Seq[IntraOrgDependency]], allProjects: Seq[CombinedProjectData], investigate: List[ProjectName])
    : (Seq[CombinedProjectData], Map[ProjectName, Seq[IntraOrgDependency]]) = {
      if (investigate.isEmpty)
        (allProjects, allDeps)
      else {
        val next :: rest = investigate
        if (allDeps.contains(next))
          go(allDeps, allProjects, rest)
        else {
          val (project, deps) = investigateProject(next)
          go(allDeps + (next -> deps), allProjects :+ project, rest ++ deps.map(_.child.name))
        }
      }
    }

    val (allProjects, allDeps) = go(Map(), Seq(), startingProjects.toList)

    (allProjects, allDeps.values.flatten.toSeq)
  }


  val (projects, edges) = findAllDependencies(startingProjects)
  val r = GraphViz.makeAPicture(OutputName, edges.map(GraphVizInterop.dependencyEdge), GraphVizInterop.projectNodeId, projects.map(GraphVizInterop.projectNode))
  println(s"There is a picture for you in ${r.getAbsolutePath}")

  val n = Neo4J.makeAPicture(edges.map(Neo4JInterop.dependencyEdge), projects.map(Neo4JInterop.projectNode), runId)

  val localProjects = projects.collect { case FoundProject(a, _) => a }
  val localProjectNames = localProjects.map(_.name)

  /* If given a base project, build scripts only on the graph from there to the single top-level project */
  val simpleEdges = baseProject match {
    case None => edges.map { case dep => (dep.parent.name, dep.child.name) }
    case Some(_) if startingProjects.size > 1 => ??? // not handled. not hard, also not needed
    case Some(base) => neo.Queries.findConnectingEdges(startingProjects.head, base).map { case dep => (dep.parent, dep.child) }
  }

  val buildOrder = Linearize.tsort(simpleEdges).toList.reverse
  println(s"Here is an order for building: ${buildOrder}")
  val buildThese = buildOrder.filter(localProjectNames.contains(_))


  Seq(
    "do" -> "$@",
    "install" -> "mvn install $*",
    "clean" -> "mvn clean",
    "fetch" -> "git fetch && ( git merge --ff-only || echo \"not able to fast-forward merge\" )",
    "newbranch" -> "git checkout $1 2>/dev/null || git checkout -b $1",
    "verify_on_master" -> """[[ $(git rev-parse --abbrev-ref HEAD) == "master" ]] && [[ -z "$(git status --porcelain)" ]]"""
  ).foreach { case (name, command) =>
    val scriptName = s"${name}_all"
    val buildScript = BuildScript.createBuildScript(BuildFileLocation, command, scriptName, buildThese)
    println(s"There is a script for you in $buildScript")
  }

  if (baseProject.isDefined) {
    val base = localProjects.find(_.name == baseProject.get).getOrElse(throw new RuntimeException(s"I can't find base project ${baseProject}"))
    val localVersionsScript = BuildScript.createScriptToLinkLocalVersions(BuildFileLocation, MavenGroup, simpleEdges, localProjects, base)
    println(s"There is a script for you in $localVersionsScript")
  }

}


object Linearize {

  // https://gist.github.com/ThiporKong/4399695
  def tsort[A](edges: Traversable[(A, A)]): Iterable[A] = {
    @tailrec
    def tsort(toPreds: Map[A, Set[A]], done: Iterable[A]): Iterable[A] = {
      val (noPreds, hasPreds) = toPreds.partition {
        _._2.isEmpty
      }
      if (noPreds.isEmpty) {
        if (hasPreds.isEmpty) done else sys.error(hasPreds.toString)
      } else {
        val found = noPreds.keys
        tsort(hasPreds.mapValues {
          _ -- found
        }, done ++ found)
      }
    }

    val toPred = edges.foldLeft(Map[A, Set[A]]()) { (acc, e) =>
      acc + (e._1 -> acc.getOrElse(e._1, Set())) + (e._2 -> (acc.getOrElse(e._2, Set()) + e._1))
    }
    tsort(toPred, Seq())
  }
}
