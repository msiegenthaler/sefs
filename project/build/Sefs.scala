import sbt._

class SefsProject(info: ProjectInfo) extends DefaultProject(info) with AutoCompilerPlugins {
  val cont = compilerPlugin("org.scala-lang.plugins" % "continuations" % "2.9.0.RC1")
  override def compileOptions = CompileOption("-P:continuations:enable") ::
//    CompileOption("-Xplugin:/Users/ms/scala/eclipse-workspace/sefs/plugin/iopreserver.jar") ::
//    CompileOption("-Xpluginsdir2:\"/Users/ms/scala/eclipse-workspace/sefs/plugin\"") ::
    CompileOption("-Xplugin-require:iopreserver") ::
    CompileOption("-Xplugin-list") ::
//    CompileOption("-Ybrowse:1") ::
    super.compileOptions

  override def packageSrcJar = defaultJarPath("-sources.jar")
  val sourceArtifact = Artifact.sources(artifactID)
  override def packageToPublishActions = super.packageToPublishActions ++ Seq(packageSrc)

  val inventsoftReleases = "Inventsoft Release Repository" at "http://mavenrepo.inventsoft.ch/repo"
  val inventsoftSnapshots = "Inventsoft Snapshot Repository" at "http://mavenrepo.inventsoft.ch/snapshot-repo"
  val jbossRepo = "JBoss public repository" at "http://repository.jboss.org/nexus/content/groups/public-jboss/"
  override def managedStyle = ManagedStyle.Maven
  val publishTo = Resolver.sftp("Inventsoft Publish", "foxtrot.inventsoft.ch", "/inventsoft/dev/mavenrepo/snapshot-repo")
  Credentials(Path.userHome / ".ivy2" / ".credentials", log)

  val scalaToolsSnapshots = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"

  val scalazCore = "org.scalaz" %% "scalaz-core" % "6.0-SNAPSHOT"
}
