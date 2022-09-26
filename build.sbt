Global / onChangedBuildSource := ReloadOnSourceChanges

inThisBuild(
  List(
    organization := "com.kubukoz",
    homepage := Some(url("https://github.com/kubukoz/drops")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        "kubukoz",
        "Jakub Kozłowski",
        "kubukoz@gmail.com",
        url("https://kubukoz.com"),
      )
    ),
  )
)

ThisBuild / scalaVersion := "2.12.17"
(ThisBuild / crossScalaVersions) := Seq("2.12.17", "2.13.9", "3.2.0")

ThisBuild / githubWorkflowTargetTags := Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches := List(RefPredicate.StartsWith(Ref.Tag("v")), RefPredicate.Equals(Ref.Branch("main")))

ThisBuild / githubWorkflowPublishPreamble := Seq(
  WorkflowStep.Use(UseRef.Public("olafurpg", "setup-gpg", "v3"))
)

ThisBuild / githubWorkflowPublish := Seq(WorkflowStep.Sbt(List("ci-release")))

ThisBuild / githubWorkflowEnv ++= List(
  "PGP_PASSPHRASE",
  "PGP_SECRET",
  "SONATYPE_PASSWORD",
  "SONATYPE_USERNAME",
).map { envKey =>
  envKey -> s"$${{ secrets.$envKey }}"
}.toMap

def crossPlugin(x: sbt.librarymanagement.ModuleID) = compilerPlugin(x.cross(CrossVersion.full))

val addCompilerPlugins = libraryDependencies ++= List(
  crossPlugin("org.polyvariant" % "better-tostring" % "0.3.17")
) ++ {
  if (scalaVersion.value.startsWith("3")) Nil
  else
    List(
      crossPlugin("org.typelevel" % "kind-projector" % "0.13.2")
    )
}

val commonSettings = Seq(
  scalacOptions -= "-Xfatal-warnings",
  addCompilerPlugins,
)

val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .settings(name := "drops-core")
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "2.8.0",
      "org.scalameta" %%% "munit" % "1.0.0-M6" % Test,
    ),
  )

val root =
  project
    .in(file("."))
    .aggregate(core.componentProjects.map(_.project): _*)
    .settings(publish := false)
