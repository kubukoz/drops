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

(ThisBuild / scalaVersion) := "2.12.14"
(ThisBuild / crossScalaVersions) := Seq("2.12.14", "2.13.6", "3.0.0")

val GraalVM11 = "graalvm-ce-java11@20.3.0"
ThisBuild / githubWorkflowJavaVersions := Seq(GraalVM11)
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
  crossPlugin("com.kubukoz" % "better-tostring" % "0.3.3")
) ++ {
  if (scalaVersion.value.startsWith("3")) Nil
  else
    List(
      crossPlugin("org.typelevel" % "kind-projector" % "0.13.0")
    )
}

val commonSettings = Seq(
  scalacOptions -= "-Xfatal-warnings",
  addCompilerPlugins,
)

val core = project
  .settings(name := "drops-core")
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.6.1"
    ),
  )

val root =
  project
    .in(file("."))
    .aggregate(core)
    .settings(publish := false)
