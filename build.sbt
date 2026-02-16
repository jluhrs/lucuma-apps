import Dependencies.*
import Versions.*
import _root_.cats.effect.kernel.syntax.resource
import com.github.sbt.git.SbtGit.GitKeys.*
import org.scalajs.linker.interface.ModuleSplitStyle
import org.typelevel.sbt.gha.PermissionValue
import org.typelevel.sbt.gha.Permissions
import sbt.Keys.*
import sbt.nio.file.FileTreeView

import scala.sys.process.*

import NativePackagerHelper.*

name := "lucuma-apps"

ThisBuild / githubWorkflowPublishTargetBranches := Seq(
  RefPredicate.StartsWith(Ref.Tag("v")),
  RefPredicate.Equals(Ref.Branch("main"))
)

ThisBuild / description        := "Lucuma Apps"
Global / onChangedBuildSource  := ReloadOnSourceChanges
ThisBuild / turbo              := true
ThisBuild / scalaVersion       := "3.8.1"
ThisBuild / crossScalaVersions := Seq("3.8.1")
ThisBuild / scalacOptions ++= Seq("-language:implicitConversions", "-explain-cyclic")
ThisBuild / scalacOptions ++= Seq(
  // ScalablyTyped macros introduce deprecated methods, this silences those warnings
  "-Wconf:msg=linkingInfo in package scala.scalajs.runtime is deprecated:s"
)

// TODO REMOVE ONCE THIS WORKS AGAIN
ThisBuild / tlCiScalafmtCheck := false
ThisBuild / tlCiScalafixCheck := false

ThisBuild / lucumaCssExts += "svg"

ThisBuild / resolvers := List(Resolver.mavenLocal)

// Gemini repository
ThisBuild / resolvers += "Gemini Repository".at(
  "https://github.com/gemini-hlsw/maven-repo/raw/master/releases"
)

ThisBuild / evictionErrorLevel := Level.Info

ThisBuild / lucumaCoverage               := false
ThisBuild / githubWorkflowArtifactUpload := false // Necessary when disabling coverage.

// Uncomment for local gmp testing
// ThisBuild / resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

enablePlugins(GitBranchPrompt)

// Build JS module for deployment, only used for observe web client
val buildJsModule = taskKey[File]("Build JS module for deployment")

lazy val esModule = Seq(
  scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
  Compile / fastLinkJS / scalaJSLinkerConfig ~= { _.withSourceMap(false) },
  Compile / fullLinkJS / scalaJSLinkerConfig ~= { _.withSourceMap(false) },
  Compile / fastLinkJS / scalaJSLinkerConfig ~= (_.withModuleSplitStyle(
    ModuleSplitStyle.FewestModules
  )),
  Compile / fullLinkJS / scalaJSLinkerConfig ~= (_.withModuleSplitStyle(
    ModuleSplitStyle.FewestModules
  ))
)

//////////////
// Projects
//////////////

lazy val root = tlCrossRootProject.aggregate(
  schemas_model,
  schemas_testkit,
  schemas_tests,
  schemas_lib,
  ui_lib,
  ui_testkit,
  ui_tests,
  ui_css,
  ui_demo,
  explore_model,
  explore_modelTests,
  explore_common,
  explore_app,
  explore_workers,
  observe_web_server,
  observe_web_client,
  observe_server,
  observe_model,
  observe_ui_model,
  navigate_epics,
  navigate_stateengine,
  navigate_server,
  navigate_web_server,
  navigate_model,
  navigate_schema_util
)

// BEGIN SCHEMAS

// For publishing packages to NPM
lazy val createNpmProject = taskKey[Unit]("Create NPM project, package.json and files")
lazy val npmPublish       = taskKey[Unit]("Run npm publish")

lazy val schemas_model =
  crossProject(JVMPlatform, JSPlatform)
    .crossType(CrossType.Pure)
    .in(file("schemas/model"))
    .settings(
      name := "lucuma-schemas-model",
      libraryDependencies ++=
        Circe.value ++
          CirceRefined.value ++
          Kittens.value ++
          LucumaCore.value ++
          LucumaOdbSchema.value ++
          LucumaItcClient.value ++
          LucumaRefined.value
    )

lazy val schemas_testkit =
  crossProject(JVMPlatform, JSPlatform)
    .crossType(CrossType.Pure)
    .in(file("schemas/testkit"))
    .dependsOn(schemas_model)
    .settings(
      name := "lucuma-schemas-testkit",
      libraryDependencies ++= LucumaCore.value ++ LucumaCoreTestkit.value
    )

lazy val schemas_tests =
  crossProject(JVMPlatform, JSPlatform)
    .crossType(CrossType.Full)
    .in(file("schemas/tests"))
    .dependsOn(schemas_testkit)
    .settings(
      libraryDependencies ++=
        In(Test)(
          MUnit.value ++
            Discipline.value
        )
    )

lazy val schemas_lib =
  crossProject(JVMPlatform, JSPlatform)
    .crossType(CrossType.Pure)
    .in(file("schemas/lib"))
    .dependsOn(schemas_model)
    .enablePlugins(CluePlugin)
    .settings(
      name                          := "lucuma-schemas",
      libraryDependencies ++=
        In(Test)(
          Fs2Io.value ++
            MUnit.value ++
            MUnitCatsEffect.value
        ),
      Compile / clueSourceDirectory := (ThisBuild / baseDirectory).value / "schemas" / "lib" / "src" / "clue",
      // Include schema files in jar.
      Compile / unmanagedResourceDirectories += (Compile / clueSourceDirectory).value / "resources",
      createNpmProject              := {
        val npmDir = target.value / "npm"

        val odbSchemaFile: File          =
          (Compile / clueSourceDirectory).value / "resources" / "lucuma" / "schemas" / "ObservationDB.graphql"
        val navigateSchemaFile: File     =
          (Compile / crossProjectBaseDirectory).value / "../../navigate/web/server/src/main/resources/navigate.graphql"
        val semVerWithPrerelease: String = // Just keep X.Y.Z from the latest tag
          gitDescribedVersion.value.getOrElse("0.0.0").takeWhile(c => c != '+' && c != '-') +
            "-" + version.value

        IO.write(
          npmDir / "package.json",
          s"""|{
             |  "name": "lucuma-schemas",
             |  "version": "$semVerWithPrerelease",
             |  "license": "${licenses.value.head._1}",
             |  "exports": {
             |    "./odb": "./${odbSchemaFile.getName}",
             |    "./navigate": "./${navigateSchemaFile.getName}"
             |  },
             |  "repository": {
             |    "type": "git",
             |    "url": "git+https://github.com/gemini-hlsw/lucuma-apps.git"
             |  }
             |}
             |""".stripMargin
        )

        IO.copyFile(odbSchemaFile, npmDir / odbSchemaFile.getName)

        // Replace the import path to the schema file to match the NPM package structure
        val navigateSchemaContent = IO
          .read(
            navigateSchemaFile
          )
          .replace(
            "from \"lucuma/schemas/ObservationDB.graphql\"",
            "from \"./ObservationDB.graphql\""
          )
        IO.write(
          npmDir / navigateSchemaFile.getName,
          navigateSchemaContent
        )

        streams.value.log.info(s"Created NPM project in ${npmDir}")
      },
      npmPublish                    := npmPublishForDir("npm").value
    )
    .jsSettings(
      Test / scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
    )

// BEGIN UI

lazy val ui_lib =
  project
    .in(file("ui/lib"))
    .dependsOn(schemas_lib.js)
    .enablePlugins(ScalaJSPlugin)
    .settings(
      name := "lucuma-ui",
      libraryDependencies ++=
        Cats.value ++
          CatsTime.value ++
          Kittens.value ++
          Mouse.value ++
          ScalaJsReact.value ++
          LucumaCore.value ++
          LucumaAgs.value ++
          LucumaReact.value ++
          LucumaPrimeStyles.value ++
          Monocle.value ++
          Crystal.value ++
          PPrint.value ++
          Fs2Dom.value ++
          Http4sCore.value ++
          Http4sCirce.value ++
          Http4sDom.value ++
          CatsRetry.value ++
          Circe.value ++
          LucumaSsoFrontendClient.value
    )

lazy val ui_testkit =
  project
    .in(file("ui/testkit"))
    .dependsOn(ui_lib, schemas_testkit.js)
    .enablePlugins(ScalaJSPlugin)
    .settings(
      name := "lucuma-ui-testkit",
      libraryDependencies ++=
        LucumaCoreTestkit.value
    )

lazy val ui_tests =
  project
    .in(file("ui/tests"))
    .dependsOn(ui_testkit)
    .enablePlugins(ScalaJSPlugin)
    .settings(
      libraryDependencies ++=
        In(Test)(
          LucumaCoreTestkit.value ++
            MUnit.value ++
            Discipline.value
        )
    )

lazy val ui_css = project
  .in(file("ui/css"))
  .dependsOn(ui_lib)
  .enablePlugins(LucumaCssPlugin)
  .settings(
    createNpmProject := {
      val _      = (Compile / lucumaCss).value
      val cssDir = target.value / "lucuma-css"
      IO.write(
        cssDir / "package.json",
        s"""|{
          |  "name": "lucuma-ui-css",
          |  "version": "${gitDescribedVersion.value.getOrElse("0.0.0")}",
          |  "license": "${licenses.value.head._1}",
          |  "repository": {
          |    "type": "git",
          |    "url": "git+https://github.com/gemini-hlsw/lucuma-ui.git"
          |  }
          |}
          |""".stripMargin
      )
      streams.value.log.info(s"Created NPM project in ${cssDir}")
    },
    npmPublish       := npmPublishForDir("lucuma-css").value
  )

lazy val ui_demo =
  project
    .in(file("observe/ui/demo"))
    .enablePlugins(ScalaJSPlugin, LucumaCssPlugin)
    .dependsOn(ui_lib, ui_css)
    .settings(
      Compile / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
      Compile / fastLinkJS / scalaJSLinkerConfig ~= (_.withModuleSplitStyle(
        ModuleSplitStyle.FewestModules
      )),
      libraryDependencies ++=
        ScalaJsReact.value ++
          Log4CatsLogLevel.value ++
          LucumaReact.value,
      Keys.test := {}
    )

// BEGIN EXPLORE

lazy val exploreCommonSettings = lucumaGlobalSettings ++ Seq(
  scalacOptions ~= (_.filterNot(Set("-Vtype-diffs")))
)

lazy val exploreCommonLibSettings = Seq(
  libraryDependencies ++=
    Cats.value ++
      CatsEffect.value ++
      CatsRetry.value ++
      Circe.value ++
      Clue.value ++
      Crystal.value ++
      Fs2.value ++
      Http4sCore.value ++
      Kittens.value ++
      LucumaCore.value ++
      LucumaHorizons.value ++
      LucumaOdbSchema.value ++
      LucumaAgs.value ++
      LucumaItcClient.value ++
      Monocle.value ++
      Mouse.value ++
      Boopickle.value ++
      In(Test)(
        MUnit.value ++
          MUnitScalaCheck.value ++
          Discipline.value ++
          CatsTimeTestkit.value ++
          CatsEffectTestkit.value ++
          MUnitCatsEffect.value ++
          MonocleLaw.value
      ),
  // temporary? fix for upgrading to Scala 3.7
  libraryDependencies += "org.scala-lang" %% "scala3-library" % scalaVersion.value,
  testFrameworks += new TestFramework("munit.Framework")
)

lazy val exploreTestkitLibSettings = Seq(
  libraryDependencies ++= Discipline.value ++
    MonocleLaw.value ++
    CatsTimeTestkit.value ++
    CatsEffectTestkit.value ++
    LucumaCoreTestkit.value ++
    LucumaCatalogTestkit.value
)

lazy val exploreCommonJvmSettings = Seq(
  libraryDependencies ++=
    Fs2Io.value
)

lazy val exploreCommonJsLibSettings =
  exploreCommonLibSettings ++ Seq(
    libraryDependencies ++=
      ClueScalaJs.value ++
        Http4sDom.value ++
        Fs2Dom.value ++
        Log4Cats.value ++
        Log4CatsLogLevel.value ++
        ScalaCollectionContrib.value ++
        ScalaJsReact.value ++
        ScalaJSDom.value ++
        In(Test)(ScalaJsReactTest.value),
    dependencyOverrides ++= ScalaJsReact.value
  )

lazy val exploreCommonModuleTest = Seq(
  Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
)

lazy val explore_model = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("explore/model"))
  .dependsOn(schemas_lib)
  .settings(exploreCommonSettings: _*)
  .settings(exploreCommonLibSettings: _*)
  .jvmSettings(exploreCommonJvmSettings)
  .jsSettings(exploreCommonJsLibSettings)

lazy val explore_modelTestkit = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("explore/model-testkit"))
  .dependsOn(explore_model, schemas_testkit)
  .settings(exploreCommonSettings: _*)
  .settings(exploreCommonLibSettings: _*)
  .settings(exploreTestkitLibSettings: _*)
  .jsSettings(exploreCommonModuleTest: _*)
  .jvmSettings(exploreCommonJvmSettings)

lazy val explore_modelTests = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("explore/model-tests"))
  .dependsOn(explore_modelTestkit)
  .settings(exploreCommonSettings: _*)
  .settings(exploreCommonLibSettings: _*)
  .jsSettings(exploreCommonModuleTest: _*)
  .jvmSettings(exploreCommonJvmSettings)

lazy val explore_workers = project
  .in(file("explore/workers"))
  .dependsOn(explore_model.js)
  .enablePlugins(ScalaJSPlugin)
  .settings(exploreCommonSettings: _*)
  .settings(exploreCommonJsLibSettings: _*)
  .settings(exploreCommonLibSettings: _*)
  .settings(esModule: _*)
  .settings(
    libraryDependencies ++= LucumaCatalog.value ++
      Http4sDom.value ++
      Log4Cats.value,
    Test / scalaJSLinkerConfig ~= {
      import org.scalajs.linker.interface.OutputPatterns
      _.withOutputPatterns(OutputPatterns.fromJSFile("%s.mjs"))
    }
  )

lazy val explore_common = project
  .in(file("explore/common"))
  .dependsOn(
    explore_model.js,
    ui_lib,
    schemas_lib.js,
    explore_modelTestkit.js % Test,
    ui_testkit              % Test
  )
  .enablePlugins(ScalaJSPlugin, BuildInfoPlugin)
  .settings(exploreCommonSettings: _*)
  .settings(exploreCommonJsLibSettings: _*)
  .settings(exploreCommonModuleTest: _*)
  .settings(
    libraryDependencies ++=
      LucumaSsoFrontendClient.value ++
        LucumaCatalog.value ++
        LucumaReact.value,
    buildInfoKeys    := Seq[BuildInfoKey](
      scalaVersion,
      sbtVersion,
      git.gitHeadCommit,
      "buildDateTime" -> System.currentTimeMillis()
    ),
    buildInfoPackage := "explore"
  )

lazy val explore_app: Project = project
  .in(file("explore/app"))
  .dependsOn(explore_model.js, explore_common)
  .enablePlugins(ScalaJSPlugin, LucumaCssPlugin, CluePlugin)
  .settings(exploreCommonSettings: _*)
  .settings(exploreCommonJsLibSettings: _*)
  .settings(esModule: _*)
  .settings(
    Test / test          := {},
    coverageEnabled      := false,
    libraryDependencies ++=
      GeminiLocales.value ++
        LucumaReact.value,
    // Build workers when you build explore
    Compile / fastLinkJS := (Compile / fastLinkJS)
      .dependsOn(explore_workers / Compile / fastLinkJS)
      .value,
    Compile / fullLinkJS := (Compile / fullLinkJS)
      .dependsOn(explore_workers / Compile / fullLinkJS)
      .value,
    buildJsModule        := {
      val jsFiles = (Compile / fullLinkJSOutput).value
      if (sys.env.getOrElse("POST_STAGE_CLEAN", "false").equals("true")) {
        println("Cleaning up...")
        // Remove coursier cache
        val coursierCacheDir = csrCacheDirectory.value
        sbt.IO.delete(coursierCacheDir)
      }
      jsFiles
    }
  )

// BEGIN OBSERVE

lazy val observeCommonSettings = Seq(
  Compile / packageDoc / mappings := Seq(),
  Compile / doc / sources         := Seq.empty,
  testFrameworks += new TestFramework("munit.Framework")
)

lazy val observe_web_server = project
  .in(file("observe/web/server"))
  .dependsOn(observe_server)
  .dependsOn(observe_model.jvm % "compile->compile;test->test")
  .enablePlugins(BuildInfoPlugin)
  .settings(observeCommonSettings: _*)
  .settings(
    libraryDependencies ++=
      UnboundId.value ++
        LucumaSsoBackendClient.value ++
        JwtCore.value ++
        JwtCirce.value ++
        Http4sServer.value ++
        Log4CatsNoop.value ++
        Http4sJdkClient.value ++
        Http4sServer.value ++
        PureConfig.value ++
        Logback.value ++
        JuliSlf4j.value,
    // Supports launching the server in the background
    reStart / mainClass := Some("observe.web.server.http4s.WebServerLauncher")
  )
  .settings(
    buildInfoUsePackageAsPath := true,
    buildInfoKeys ++= Seq[BuildInfoKey](name, version, buildInfoBuildNumber),
    buildInfoOptions += BuildInfoOption.BuildTime,
    buildInfoObject           := "OcsBuildInfo",
    buildInfoPackage          := "observe.web.server"
  )

lazy val observe_ui_model = project
  .in(file("observe/web/client-model"))
  .dependsOn(ui_lib, schemas_lib.js, observe_model.js, ui_testkit % Test)
  .enablePlugins(ScalaJSPlugin)
  .settings(lucumaGlobalSettings: _*)
  .settings(
    coverageEnabled := false,
    libraryDependencies ++=
      Crystal.value ++
        LucumaCore.value ++
        Circe.value ++
        In(Test)(
          MUnit.value ++
            Discipline.value ++
            CrystalTestkit.value
        )
  )

lazy val observe_web_client = project
  .in(file("observe/web/client"))
  .dependsOn(ui_lib, schemas_lib.js, observe_model.js, observe_ui_model)
  .enablePlugins(ScalaJSPlugin, LucumaCssPlugin, CluePlugin, BuildInfoPlugin, NoPublishPlugin)
  .settings(lucumaGlobalSettings: _*)
  .settings(esModule: _*)
  .settings(
    Test / test      := {},
    coverageEnabled  := false,
    libraryDependencies ++=
      Kittens.value ++
        Clue.value ++
        ClueScalaJs.value ++
        Fs2.value ++
        Http4sClient.value ++
        Http4sDom.value ++
        Crystal.value ++
        ScalaJsReact.value ++
        Cats.value ++
        CatsEffect.value ++
        LucumaReact.value ++
        Monocle.value ++
        LucumaCore.value ++
        Log4CatsLogLevel.value,
    scalacOptions ~= (_.filterNot(Set("-Vtype-diffs"))),
    buildInfoKeys    := Seq[BuildInfoKey](
      scalaVersion,
      sbtVersion,
      git.gitHeadCommit,
      "buildDateTime" -> System.currentTimeMillis()
    ),
    buildInfoPackage := "observe.ui",
    Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
  )
  .settings(
    buildJsModule / fileInputs += (Compile / fullLinkJS / scalaJSLinkerOutputDirectory).value.toGlob,
    buildJsModule := {
      if ((Process("pnpm" :: "build" :: Nil, baseDirectory.value) !) != 0)
        throw new Exception("Error building web client")
      else
        baseDirectory.value / "deploy" // Must match directory declared in vite.config.mts
    },
    buildJsModule := buildJsModule.dependsOn(Compile / fullLinkJS).value
  )

lazy val observe_server = project
  .in(file("observe/server_new"))
  .dependsOn(schemas_lib.jvm)
  .dependsOn(observe_model.jvm % "compile->compile;test->test")
  .enablePlugins(BuildInfoPlugin, CluePlugin)
  .settings(observeCommonSettings: _*)
  .settings(
    libraryDependencies ++=
      Http4sCirce.value ++
        Http4sXml.value ++
        Log4Cats.value ++
        PPrint.value ++
        Clue.value ++
        ClueHttp4s.value ++
        ClueNatchez.value ++
        CatsParse.value ++
        Acm.value ++
        GiapiScala.value ++
        Coulomb.value ++
        Http4sServer.value ++
        Http4sJdkClient.value ++
        PureConfig.value ++
        Monocle.value ++
        Circe.value ++
        Natchez.value ++
        CatsEffect.value ++
        In(Test)(
          MUnit.value ++
            Log4CatsNoop.value
        ),
    headerSources / excludeFilter := HiddenFileFilter || (file(
      "observe/server_new"
    ) / "src/main/scala/pureconfig/module/http4s/package.scala").getName
  )
  .settings(
    buildInfoUsePackageAsPath := true,
    buildInfoKeys ++= Seq[BuildInfoKey](name, version),
    buildInfoObject           := "OcsBuildInfo",
    buildInfoPackage          := "observe.server"
  )
  .settings(
    unmanagedSources / excludeFilter := (unmanagedSources / excludeFilter).value
      || (Compile / sourceDirectory).value + "/scala/observe/server/flamingos2/*"
      || (Compile / sourceDirectory).value + "/scala/observe/server/ghost/*"
      || (Compile / sourceDirectory).value + "/scala/observe/server/gnirs/*"
      || (Compile / sourceDirectory).value + "/scala/observe/server/gpi/*"
      || (Compile / sourceDirectory).value + "/scala/observe/server/gsaoi/*"
      || (Compile / sourceDirectory).value + "/scala/observe/server/nifs/*"
      || (Compile / sourceDirectory).value + "/scala/observe/server/niri/*"
  )

// Unfortunately crossProject doesn't seem to work properly at the module/build.sbt level
// We have to define the project properties at this level
lazy val observe_model = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("observe/model"))
  .settings(
    libraryDependencies ++=
      Mouse.value ++
        CatsTime.value ++
        Http4sCore.value ++
        Http4sCirce.value ++
        Http4sLaws.value ++
        LucumaOdbSchema.value ++
        Coulomb.value ++
        Monocle.value ++
        LucumaCore.value ++
        Circe.value ++
        In(Test)(
          MUnit.value ++
            CoulombTestkit.value ++
            Discipline.value ++
            CatsEffectLaws.value ++
            CatsEffectTestkit.value
        )
  )
  .jvmSettings(observeCommonSettings)
  .jsSettings(
    // And add a custom one
    libraryDependencies ++=
      JavaTimeJs.value,
    // In(Test)(LucumaUITestkit.value),
    coverageEnabled := false
  )

/**
 * Mappings common to applications, including configurations and web application.
 */
lazy val observeDeployedAppMappings = Seq(
  Universal / mappings ++= {
    val clientDir: File                         = (observe_web_client / buildJsModule).value
    val clientMappings: Seq[(File, String)]     =
      directory(clientDir).flatMap(path =>
        // Don't include environment confs, if present.
        if (path._2.endsWith(".conf.json")) None
        else Some(path._1 -> ("app/" + path._1.relativeTo(clientDir).get.getPath))
      )
    val siteConfigDir: File                     = (ThisProject / baseDirectory).value / "conf"
    val siteConfigMappings: Seq[(File, String)] = directory(siteConfigDir).map(path =>
      path._1 -> ("conf/" + path._1.relativeTo(siteConfigDir).get.getPath)
    )
    clientMappings ++ siteConfigMappings
  }
)

/**
 * Settings for Observe in Linux
 */
lazy val observeLinux = Seq(
  // User/Group for execution
  Linux / daemonUser     := "software",
  Linux / daemonGroup    := "software",
  Universal / maintainer := "Software Group <software@gemini.edu>",
  // This lets us build RPMs from snapshot versions
  Linux / name           := "Observe Server",
  Linux / version        :=
    (ThisBuild / version).value.replace("-SNAPSHOT", "").replace("-", "_").replace(" ", "")
)

/**
 * Project for the observe server app for development
 */
lazy val observe_deploy = project
  .in(file("observe/deploy"))
  .dependsOn(observe_web_server)
  .enablePlugins(LucumaDockerPlugin, JavaServerAppPackaging)
  .settings(observeDeployedAppMappings: _*)
  .settings(observeCommonSettings: _*)
  .settings(
    description          := "Observe Server",
    Docker / packageName := "gpp-obs",
    // Main class for launching
    Compile / mainClass  := Some("observe.web.server.http4s.WebServerLauncher"),
    dockerExposedPorts ++= Seq(9090, 9091), // Must match deployed app.conf web-server.port
    // Name of the launch script
    executableScriptName     := "observe-server",
    // Specify a different name for the config file
    bashScriptConfigLocation := Some("${app_home}/../conf/launcher.args"),
    bashScriptExtraDefines += """addJava "-Dlogback.configurationFile=${app_home}/../conf/$SITE/logback.xml""""
  )

// BEGIN NAVIGATE

lazy val navigateCommonSettings = Seq(
  // Workaround for https://github.com/sbt/sbt/issues/4109
  initialCommands += "jline.TerminalFactory.get.init\n",
  Compile / doc / scalacOptions ++= Seq(
    "-groups",
    "-sourcepath",
    (LocalRootProject / baseDirectory).value.getAbsolutePath,
    "-skip-packages",
    "scalaz",
    "-doc-title",
    "Gem",
    "-doc-version",
    version.value
  ),
  // Common libraries
  libraryDependencies ++= In(Test)(CatsTestkitScalaTest.value),
  // Don't build javadoc when we're packaging the docker image.
  Compile / packageDoc / mappings := Seq(),
  Compile / doc / sources         := Seq.empty,

  // We don't care to see updates about the scala language itself
  dependencyUpdatesFilter -= moduleFilter(name = "scala-library"),
  dependencyUpdatesFilter -= moduleFilter(name = "scala-reflect"),
  // Don't worry about stale deps pulled in by scala-js
  dependencyUpdatesFilter -= moduleFilter(organization = "org.eclipse.jetty"),
  // Don't worry about old ocs related dependencies
  dependencyUpdatesFilter -= moduleFilter(organization = "dom4j"),
  dependencyUpdatesFilter -= moduleFilter(organization = "net.sf.opencsv"),
  dependencyUpdatesFilter -= moduleFilter(organization = "commons-httpclient"),
  Test / testOptions += Tests.Argument(
    TestFrameworks.ScalaTest,
    "-l",
    "gem.test.Tags.RequiresNetwork"
  ), // by default, ignore network tests
  // Don't worry about monocle versions that start with the same prefix.
  dependencyUpdatesFilter -= moduleFilter(
    organization = "com.github.julien-truffaut",
    revision = sbt.io.GlobFilter(Versions.monocle.replace("-cats", "*"))
  ),
  testFrameworks += new TestFramework("munit.Framework")
)

lazy val navigate_epics = project
  .in(file("navigate/epics"))
  .settings(
    name                     := "navigate-epics",
    libraryDependencies ++=
      Cats.value ++
        CatsEffect.value ++
        Mouse.value ++
        Fs2.value ++
        EpicsCa.value ++
        LucumaCore.value ++
        In(Test)(
          MUnit.value ++
            MUnitCatsEffect.value ++
            EpicsJca.value
        ),
    Test / parallelExecution := false
  )

lazy val navigate_stateengine = project
  .in(file("navigate/stateengine"))
  .settings(
    name := "navigate-stateengine",
    libraryDependencies ++=
      Cats.value ++
        CatsEffect.value ++
        Mouse.value ++
        Fs2.value ++
        In(Test)(
          MUnit.value ++
            MUnitCatsEffect.value ++
            CatsLaws.value
        )
  )

lazy val navigate_web_server = project
  .in(file("navigate/web/server"))
  .dependsOn(
    schemas_lib.jvm,
    navigate_schema_util,
    navigate_server,
    navigate_model % "compile->compile;test->test"
  )
  .enablePlugins(BuildInfoPlugin, GitBranchPrompt)
  .settings(navigateCommonSettings: _*)
  .settings(
    name                := "navigate_web_server",
    libraryDependencies ++=
      Log4CatsNoop.value ++
        CatsEffect.value ++
        Log4Cats.value ++
        Http4sCirce.value ++
        GraphQLRoutes.value ++
        Natchez.value ++
        Http4sJdkClient.value ++
        Http4sServer.value ++
        PureConfig.value ++
        JuliSlf4j.value ++
        Log4s.value ++
        Logback.value ++
        LucumaHorizons.value ++
        Grackle.value ++
        In(Test)(
          MUnit.value ++
            MUnitCatsEffect.value
        ),
    // Supports launching the server in the background
    reStart / mainClass := Some("navigate.web.server.http4s.WebServerLauncher"),
    // Don't include configuration files in the JAR. We want them outside, so they are editable.
    Compile / packageBin / mappings ~= {
      _.filterNot(f => f._1.getName.endsWith("logback.xml"))
    }
  )
  .settings(
    buildInfoUsePackageAsPath := true,
    buildInfoKeys ++= Seq[BuildInfoKey](name, version, buildInfoBuildNumber),
    buildInfoOptions += BuildInfoOption.BuildTime,
    buildInfoObject           := "OcsBuildInfo",
    buildInfoPackage          := "navigate.web.server"
  )

lazy val navigate_model = project
  .in(file("navigate/model"))
  .enablePlugins(GitBranchPrompt)
  .settings(
    libraryDependencies ++=
      Mouse.value ++
        Http4sCore.value ++
        CatsTime.value ++
        Monocle.value ++
        LucumaCore.value ++
        Circe.value ++
        In(Test)(MUnit.value ++ Discipline.value)
  )

lazy val navigate_schema_util = project
  .in(file("navigate/schema-util"))
  .settings(navigateCommonSettings: _*)
  .settings(
    libraryDependencies ++=
      CatsEffect.value ++
        Fs2.value ++
        Log4Cats.value ++
        LucumaCore.value ++
        Http4sClient.value ++
        Grackle.value ++
        In(Test)(MUnit.value ++ MUnitCatsEffect.value)
  )

lazy val navigate_server = project
  .in(file("navigate/server"))
  .dependsOn(
    schemas_lib.jvm,
    navigate_epics,
    navigate_stateengine,
    navigate_model % "compile->compile;test->test"
  )
  .enablePlugins(CluePlugin)
  .settings(navigateCommonSettings: _*)
  .settings(
    libraryDependencies ++=
      CatsEffect.value ++
        Fs2.value ++
        Log4Cats.value ++
        Http4sCirce.value ++
        Clue.value ++
        ClueHttp4s.value ++
        LucumaSsoBackendClient.value ++
        LucumaCore.value ++
        LucumaHorizons.value ++
        Http4sClient.value ++
        In(Test)(
          MUnit.value ++
            MUnitCatsEffect.value
        )
  )

lazy val navigateDeployedAppMappings = Seq(
  // Copy the resource directory, with customized configuration files, but first remove existing mappings.
  Universal / mappings ++= { // maps =>
    val siteConfigDir: File                     = (ThisProject / baseDirectory).value / "conf"
    val siteConfigMappings: Seq[(File, String)] = directory(siteConfigDir).map(path =>
      path._1 -> ("conf/" + path._1.relativeTo(siteConfigDir).get.getPath)
    )
    siteConfigMappings
  }
)

/**
 * Project for the navigate server app for development
 */
lazy val navigate_deploy = project
  .in(file("navigate/deploy"))
  .dependsOn(navigate_web_server)
  .enablePlugins(LucumaDockerPlugin, JavaServerAppPackaging)
  .settings(
    description              := "Navigate server",
    Docker / packageName     := "gpp-nav-server",
    // Main class for launching
    Compile / mainClass      := Some("navigate.web.server.http4s.WebServerLauncher"),
    // Name of the launch script
    executableScriptName     := "navigate-server",
    // Specify a different name for the config file
    bashScriptConfigLocation := Some("${app_home}/../conf/launcher.args"),
    bashScriptExtraDefines += """addJava "-Dlogback.configurationFile=${app_home}/../conf/$SITE/logback.xml"""",
    // Additional launch options, -J params will be added as jvm parameters
    Universal / javaOptions ++= Seq("-J-Xmx1024m", "-J-Xms256m")
  )
  .settings(navigateDeployedAppMappings: _*)
  .settings(navigateCommonSettings: _*)

// BEGIN ALIASES

def prettierCmd(fix: Boolean): String =
  s"pnpm exec prettier ${if (fix) "--write" else "--check"} ."

def styleLintCmds(mode: String, fix: Boolean, dirs: List[String]): List[String] = {
  val stylelintFixFlag = if (fix) "--fix " else ""
  // Removes all lines that don't define a variable, thus building a viable CSS file for linting. Thanks ChatGPT.
  (raw"""find ui/lib/src/main/resources/lucuma-css -maxdepth 1 -type f -exec sed -n -e '/^[[:space:]]*--.*;[[:space:]]*$$/p' -e '/^[[:space:]]*--[^;]*$$/,/;$$/p' {} + >vars.css"""
    +: dirs.map(dir => s"pnpm exec stylelint $stylelintFixFlag$dir")) :+
    "rm vars.css"
}

val cssDirs: List[String] = List(
  "explore/common/src/main/webapp/sass",
  "observe/web/client/src/main/webapp/styles",
  "ui/lib/src/main/resources/lucuma-css"
)

def allStyleLintCmds(fix: Boolean): List[String] =
  styleLintCmds("dark", fix, cssDirs) ++
    styleLintCmds("light", fix, cssDirs)

def allLintCmds(fix: Boolean): List[String] =
  allStyleLintCmds(fix) :+ prettierCmd(fix)

def runCmds(cmds: List[String]): Unit = {
  val batch: List[ProcessBuilder] = cmds.flatMap { cmd =>
    val fixedCmd: String     = cmd.replaceAll("'", "") // Quotes not needed when running from here.
    val echo: ProcessBuilder = Process(s"echo <$fixedCmd>")
    val split: List[String]  = fixedCmd.split(" >").toList
    split match {
      case exec :: fileName :: Nil => List(echo, Process(exec) #> file(fileName))
      case _                       => List(echo, Process(fixedCmd))
    }
  }
  batch.reduceLeft(_ #&& _) ! match {
    case 0 => ()
    case n => throw new Exception(s"Error in CSS format (dark), exit code $n")
  }
}

val lintCheck: TaskKey[Unit] = taskKey[Unit]("Lint style files")
lintCheck := {
  val _ = (ui_css / Compile / lucumaCss).value // Ensure Prime CSS is imported
  runCmds(allLintCmds(fix = false))
}

val lintFix: TaskKey[Unit] = taskKey[Unit]("Fix style files")
lintFix := {
  val _ = (ui_css / Compile / lucumaCss).value // Ensure Prime CSS is imported
  runCmds(allLintCmds(fix = true))
}

addCommandAlias(
  "quickTest",
  "explore_modelTestsJVM/test"
)

addCommandAlias(
  "fixImports",
  "; scalafix OrganizeImports; Test/scalafix OrganizeImports"
)

addCommandAlias(
  "fixAll",
  "; prePR; lintFix"
)

// Custom commands to facilitate web development
val startNavigateAllCommands = List(
  "navigate_web_server/reStart"
)
val stopNavigateAllCommands  = List(
  "navigate_web_server/reStop"
)

addCommandAlias("startNavigateAll", startNavigateAllCommands.mkString(";", ";", ""))
addCommandAlias("stopNavigateAll", stopNavigateAllCommands.mkString(";", ";", ""))

// BEGIN GITHUB ACTIONS

val pushCond: String                            = "github.event_name == 'push'"
val prCond: String                              = "github.event_name == 'pull_request'"
val mainCond: String                            = "github.ref == 'refs/heads/main'"
val notMainCond: String                         = "github.ref != 'refs/heads/main'"
val geminiRepoCond: String                      = "startsWith(github.repository, 'gemini')"
val isMergedCond: String                        = "github.event.pull_request.merged == true"
def changedProjectCond(project: String): String =
  s"steps.changedProjects.outputs.${project} == 'true'"
def allConds(conds: String*): String            = conds.mkString("(", " && ", ")")
def anyConds(conds: String*): String            = conds.mkString("(", " || ", ")")
val exploreChangedCond: String                  =
  anyConds(
    changedProjectCond("explore"),
    changedProjectCond("schemas"),
    changedProjectCond("ui"),
    changedProjectCond("projectDef")
  )
val observeChangedCond: String                  =
  anyConds(
    changedProjectCond("observe"),
    changedProjectCond("schemas"),
    changedProjectCond("ui"),
    changedProjectCond("projectDef")
  )
val navigateChangedCond: String                 =
  anyConds(
    changedProjectCond("navigate"),
    changedProjectCond("schemas"),
    changedProjectCond("projectDef")
  )

val faNpmAuthToken = "FONTAWESOME_NPM_AUTH_TOKEN" -> "${{ secrets.FONTAWESOME_NPM_AUTH_TOKEN }}"
val herokuToken    = "HEROKU_API_KEY"             -> "${{ secrets.HEROKU_API_KEY }}"

ThisBuild / githubWorkflowGeneratedUploadSteps := Seq.empty
ThisBuild / githubWorkflowSbtCommand           := "sbt -v -J-Xmx6g"
ThisBuild / githubWorkflowEnv += faNpmAuthToken
ThisBuild / githubWorkflowEnv += herokuToken

ThisBuild / githubWorkflowPermissions := Some(
  Permissions.Specify.defaultPermissive
    .withIdToken(PermissionValue.Write)
    .withContents(PermissionValue.Read)
)

lazy val setupPnpmAndNode = List(
  WorkflowStep.Use(
    UseRef.Public("pnpm", "action-setup", "v4"),
    name = Some("Setup pnpm")
  ),
  WorkflowStep.Use(
    UseRef.Public("actions", "setup-node", "v6"),
    name = Some("Setup Node.js"),
    params = Map(
      "node-version" -> "24",
      "registry-url" -> "https://registry.npmjs.org",
      "cache"        -> "pnpm"
    )
  )
)

lazy val rootSetupNodePnpmInstall =
  setupPnpmAndNode ++
    List(
      WorkflowStep.Run(
        List("pnpm install --frozen-lockfile --filter '!lucuma-ui-demo' --prefer-offline"),
        name = Some("pnpm install")
      )
    )

lazy val rootOnlySetupNodePnpmInstall =
  setupPnpmAndNode ++
    List(
      WorkflowStep.Run(
        List("pnpm install --frozen-lockfile --filter 'lucuma-apps' --prefer-offline"),
        name = Some("pnpm install")
      )
    )

lazy val exploreSetupNodePnpmInstall =
  setupPnpmAndNode ++ List(
    WorkflowStep.Run(
      List("pnpm install --frozen-lockfile --filter explore --prefer-offline"),
      name = Some("pnpm install")
    )
  )

lazy val observeSetupNodePnpmInstall =
  setupPnpmAndNode ++ List(
    WorkflowStep.Run(
      List("pnpm install --frozen-lockfile --filter observe --prefer-offline"),
      name = Some("pnpm install")
    )
  )

lazy val dockerHubLogin =
  WorkflowStep.Run(
    List(
      "echo ${{ secrets.DOCKER_HUB_TOKEN }} | docker login --username nlsoftware --password-stdin"
    ),
    name = Some("Login to Docker Hub")
  )

lazy val sbtDockerPublishObserve =
  WorkflowStep.Sbt(
    List("clean", "observe_deploy/docker:publish"),
    name = Some("Build and Publish Observe Docker image")
  )

lazy val sbtDockerPublishNavigate =
  WorkflowStep.Sbt(
    List("clean", "navigate_deploy/docker:publish"),
    name = Some("Build and Publish Navigate Docker image")
  )

lazy val herokuRelease =
  WorkflowStep.Run(
    List(
      "pnpm install -g heroku",
      "heroku container:login",
      "docker tag noirlab/gpp-obs registry.heroku.com/${{ vars.HEROKU_APP_NAME_GN || 'observe-dev-gn' }}/web",
      "docker push registry.heroku.com/${{ vars.HEROKU_APP_NAME_GN || 'observe-dev-gn' }}/web",
      "heroku container:release web -a ${{ vars.HEROKU_APP_NAME_GN || 'observe-dev-gn' }} -v",
      "docker tag noirlab/gpp-obs registry.heroku.com/${{ vars.HEROKU_APP_NAME_GS || 'observe-dev-gs' }}/web",
      "docker push registry.heroku.com/${{ vars.HEROKU_APP_NAME_GS || 'observe-dev-gs' }}/web",
      "heroku container:release web -a ${{ vars.HEROKU_APP_NAME_GS || 'observe-dev-gs' }} -v"
    ),
    name = Some("Deploy and release app in Heroku")
  )

lazy val exploreSbtLink =
  WorkflowStep.Sbt(List("explore_app/buildJsModule"), name = Some("Link Explore"))

lazy val exploreNpmBuild = WorkflowStep.Run(
  List("pnpm explore build"),
  name = Some("Build Explore"),
  env = Map("NODE_OPTIONS" -> "--max-old-space-size=8192")
)

// https://frontside.com/blog/2020-05-26-github-actions-pull_request/#how-does-pull_request-affect-actionscheckout
lazy val overrideCiCommit = WorkflowStep.Run(
  List("""echo "CI_COMMIT_SHA=${{ github.event.pull_request.head.sha}}" >> $GITHUB_ENV"""),
  name = Some("override CI_COMMIT_SHA"),
  cond = Some(prCond)
)

lazy val exploreBundlemon = WorkflowStep.Use(
  UseRef.Public("lironer", "bundlemon-action", "v1"),
  name = Some("Run BundleMon"),
  params = Map(
    "bundlemon-args" -> s"""--config explore/.bundlemonrc.json --subProject "Explore""""
  )
)

def firebaseDeploy(name: String, cond: String, live: Boolean) = WorkflowStep.Use(
  UseRef.Public("FirebaseExtended", "action-hosting-deploy", "v0"),
  name = Some(name),
  cond = Some(cond),
  params = Map(
    "repoToken"              -> "${{ secrets.GITHUB_TOKEN }}",
    "firebaseServiceAccount" -> "${{ secrets.FIREBASE_SERVICE_ACCOUNT_EXPLORE_GEMINI }}",
    "projectId"              -> "explore-gemini",
    "target"                 -> "dev",
    "entryPoint"             -> "./explore"
  ) ++ (if (live) Map("channelId" -> "live") else Map.empty)
)

// lazy val firebaseDeployReview = firebaseDeploy(
//   "Deploy review app to Firebase",
//   allConds(
//     prCond,
//     notBotCond,
//     "github.event.pull_request.head.repo.full_name == github.repository"
//   ),
//   live = false
// )

lazy val firebaseDeployDev = firebaseDeploy(
  "Deploy staging app to Firebase",
  allConds(mainCond, exploreChangedCond),
  live = true
)

lazy val recordDeploymentMetadata = WorkflowStep.Run(
  List(
    "# Create a deployment record with commit SHA for tracking",
    """echo "Recording deployment: ${{ github.sha }} to explore-gemini-dev"""",
    """curl -X POST https://api.github.com/repos/${{ github.repository }}/deployments -H "Authorization: Bearer ${{ secrets.GITHUB_TOKEN }}" -H "Accept: application/vnd.github+json" -d '{ "ref": "${{ github.sha }}", "environment": "development", "description": "Explore deployment to dev", "auto_merge": false, "required_contexts": [], "task": "deploy:Explore" }' """
  ),
  name = Some("Record deployment SHA"),
  cond = Some(allConds(mainCond, exploreChangedCond))
)

ThisBuild / githubWorkflowBuildPreamble ++= exploreSetupNodePnpmInstall

val usePathsFilter: WorkflowStep = WorkflowStep.Use(
  UseRef.Public("dorny", "paths-filter", "v3"),
  Map(
    "filters" ->
      """projectDef:
  - 'build.sbt'
  - 'project/**'
schemas:
  - 'schemas/**'
ui:
  - 'ui/**'
explore:
  - 'explore/**'
navigate:
  - 'navigate/**'
observe:
  - 'observe/**'"""
  ),
  "changedProjects".some
)

ThisBuild / githubWorkflowAddedJobs +=
  WorkflowJob(
    "explore-deploy",
    "Build and deploy Explore",
    githubWorkflowJobSetup.value.toList :::
      usePathsFilter ::
      exploreSetupNodePnpmInstall :::
      exploreSbtLink ::
      exploreNpmBuild ::
      overrideCiCommit ::
      exploreBundlemon ::
      // firebaseDeployReview ::
      firebaseDeployDev ::
      recordDeploymentMetadata ::
      Nil,
    // Only 1 scalaVersion, so no need for matrix
    sbtStepPreamble = Nil,
    scalas = Nil,
    javas = githubWorkflowJavaVersions.value.toList.take(1),
    cond = Some(allConds(anyConds(mainCond, prCond), geminiRepoCond))
  )

ThisBuild / githubWorkflowAddedJobs +=
  WorkflowJob(
    "observe-deploy",
    "Build and publish Observe Docker image / Deploy to Heroku",
    githubWorkflowJobSetup.value.toList :::
      observeSetupNodePnpmInstall :::
      dockerHubLogin ::
      sbtDockerPublishObserve ::
      herokuRelease ::
      Nil,
    scalas = List(scalaVersion.value),
    javas = githubWorkflowJavaVersions.value.toList.take(1),
    cond = Some(allConds(mainCond, geminiRepoCond))
  )

ThisBuild / githubWorkflowAddedJobs +=
  WorkflowJob(
    "navigate-deploy",
    "Build and publish Navigate Docker image",
    githubWorkflowJobSetup.value.toList :::
      dockerHubLogin ::
      sbtDockerPublishNavigate ::
      Nil,
    scalas = List(scalaVersion.value),
    javas = githubWorkflowJavaVersions.value.toList.take(1),
    cond = Some(allConds(mainCond, geminiRepoCond))
  )

lazy val lucumaCssStep = WorkflowStep.Sbt(List("ui_css/lucumaCss"), name = Some("Import CSS files"))

lazy val lintAllStep = WorkflowStep.Run(
  allLintCmds(fix = false),
  name = Some("Run linters")
)

ThisBuild / githubWorkflowAddedJobs +=
  WorkflowJob(
    "lint",
    "Run linters",
    githubWorkflowJobSetup.value.toList :::
      rootOnlySetupNodePnpmInstall :::
      lucumaCssStep ::
      lintAllStep ::
      Nil,
    scalas = List(scalaVersion.value),
    javas = githubWorkflowJavaVersions.value.toList.take(1),
    cond = Some(allConds(anyConds(mainCond, prCond), geminiRepoCond))
  )

ThisBuild / githubWorkflowPublishPreamble ++= setupPnpmAndNode

ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    List("ui_css/npmPublish"),
    name = Some("NPM Publish"),
    cond = Some("startsWith(github.ref, 'refs/tags/v')")
  )
)

def npmPublishForDir(dir: String) = Def.task {
  val publishDir = target.value / dir

  val _ = createNpmProject.value
  Process(List("npm", "publish", "--tag", "latest"), publishDir).!!
  streams.value.log.info(s"Published NPM package from ${publishDir}")
}

ThisBuild / mergifyLabelPaths :=
  Map(
    "schemas"  -> baseDirectory.value / "schemas",
    "ui"       -> baseDirectory.value / "ui",
    "explore"  -> baseDirectory.value / "explore",
    "navigate" -> baseDirectory.value / "navigate",
    "observe"  -> baseDirectory.value / "observe"
  )
