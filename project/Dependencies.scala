import sbt.*
import org.portablescala.sbtplatformdeps.PlatformDepsGroupArtifactID
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import sbt.librarymanagement.*
import sbt.librarymanagement.DependencyBuilders.OrganizationArtifactName
import Versions.*
// import scala.annotation.targetName // TODO Use this in sbt 2

object Dependencies {

  private def deps(modules: PlatformDepsGroupArtifactID*)(version: String): Seq[ModuleID] =
    modules.map(_ % version)

  // @targetName("depsJVM") // TODO Use this in sbt 2
  private def depsJVM(modules: OrganizationArtifactName*)(version: String): Seq[ModuleID] =
    modules.map(_ % version)

  def In(configuration: Configuration)(dependencies: Seq[ModuleID]): Seq[ModuleID] =
    dependencies.map(_ % configuration)

  val Acm = Def.setting(
    depsJVM("edu.gemini" % "acm_2.13")(acm)
  )

  val Boopickle = Def.setting(
    deps("io.suzaku" %%% "boopickle")(boopickle)
  )

  val Cats = Def.setting(
    deps("org.typelevel" %%% "cats-core")(cats)
  )

  val CatsEffect = Def.setting(
    deps("org.typelevel" %%% "cats-effect")(catsEffect)
  )

  val CatsEffectLaws = Def.setting(
    deps("org.typelevel" %%% "cats-effect-laws")(catsEffect)
  )

  val CatsEffectTestkit = Def.setting(
    deps("org.typelevel" %%% "cats-effect-testkit")(catsEffect)
  )

  val CatsLaws = Def.setting(
    deps("org.typelevel" %%% "cats-laws")(cats)
  )

  val CatsParse = Def.setting(
    deps("org.typelevel" %%% "cats-parse")(catsParse)
  )

  val CatsRetry = Def.setting(
    deps("com.github.cb372" %%% "cats-retry")(catsRetry)
  )

  val CatsTestkitScalaTest = Def.setting(
    deps("org.typelevel" %%% "cats-testkit-scalatest")(catsTestkitScalaTest)
  )

  val CatsTime = Def.setting(
    deps("org.typelevel" %%% "cats-time")(catsTime)
  )

  val CatsTimeTestkit = Def.setting(
    deps("org.typelevel" %%% "cats-time-testkit")(catsTime)
  )

  val Circe = Def.setting(
    deps(
      "io.circe" %%% "circe-core",
      "io.circe" %%% "circe-generic",
      "io.circe" %%% "circe-parser"
    )(circe)
  )

  val CirceRefined = Def.setting(
    deps("io.circe" %%% "circe-refined")(circeRefined)
  )

  val Clue = Def.setting(
    deps("edu.gemini" %%% "clue-core")(clue)
  )

  val ClueGenerator = Def.setting(
    deps("edu.gemini" %%% "clue-generator")(clue)
  )

  val ClueHttp4s = Def.setting(
    depsJVM("edu.gemini" %% "clue-http4s")(clue)
  )

  val ClueNatchez = Def.setting(
    depsJVM("edu.gemini" %% "clue-natchez")(clue)
  )

  val ClueScalaJs = Def.setting(
    deps("edu.gemini" %%% "clue-scalajs")(clue)
  )

  val Coulomb = Def.setting(
    deps(
      "com.manyangled" %%% "coulomb-core",
      "com.manyangled" %%% "coulomb-units"
    )(coulomb)
  )

  val CoulombTestkit = Def.setting(
    deps("com.manyangled" %%% "coulomb-testkit")(coulomb)
  )

  val Crystal = Def.setting(
    deps("edu.gemini" %%% "crystal")(crystal)
  )

  val CrystalTestkit = Def.setting(
    deps("edu.gemini" %%% "crystal-testkit")(crystal)
  )

  val Discipline = Def.setting(
    Seq(
      "org.typelevel" %%% "discipline-core"  % discipline,
      "org.typelevel" %%% "discipline-munit" % disciplineMUnit
    )
  )

  val EpicsCa = Def.setting(
    depsJVM("org.epics" % "ca")(epicsCa)
  )

  val EpicsJca = Def.setting(
    depsJVM("org.epics" % "jca")(epicsJca)
  )

  val Fs2 = Def.setting(
    deps("co.fs2" %%% "fs2-core")(fs2)
  )

  val Fs2Data = Def.setting(
    deps("org.gnieh" %%% "fs2-data-csv")(fs2Data)
  )

  val Fs2Io = Def.setting(
    deps("co.fs2" %%% "fs2-io")(fs2)
  )

  val Fs2Node = Def.setting(
    deps("co.fs2" %%% "fs2-node")(fs2)
  )

  val Fs2Dom = Def.setting(
    deps("com.armanbilge" %%% "fs2-dom")(fs2Dom)
  )

  val GeminiLocales = Def.setting(
    deps("edu.gemini" %%% "gemini-locales")(geminiLocales)
  )

  val GiapiScala = Def.setting(
    depsJVM("edu.gemini" %% "giapi")(giapiScala)
  )

  val Grackle = Def.setting(
    depsJVM(
      "org.typelevel" %% "grackle-core",
      "org.typelevel" %% "grackle-generic",
      "org.typelevel" %% "grackle-circe"
    )(grackle)
  )

  val GraphQLRoutes = Def.setting(
    depsJVM("edu.gemini" %% "lucuma-graphql-routes")(graphQLRoutes)
  )

  val Http4sCirce = Def.setting(
    deps("org.http4s" %%% "http4s-circe")(http4s)
  )

  val Http4sClient = Def.setting(
    deps("org.http4s" %%% "http4s-client")(http4s)
  )

  val Http4sCore = Def.setting(
    deps(
      "org.http4s" %%% "http4s-core",
      "org.http4s" %%% "http4s-client"
    )(http4s)
  )

  val Http4sDom = Def.setting(
    deps("org.http4s" %%% "http4s-dom")(http4sDom)
  )

  val Http4sJdkClient =
    Def.setting(
      depsJVM(
        "org.http4s" %% "http4s-dsl",
        "org.http4s" %% "http4s-ember-client"
      )(http4s) ++
        depsJVM("org.http4s" %% "http4s-jdk-http-client")(http4sJdkHttpClient)
    )

  val Http4sLaws = Def.setting(
    deps("org.http4s" %%% "http4s-laws")(http4s)
  )

  val Http4sServer = Def.setting(
    depsJVM(
      "org.http4s" %% "http4s-dsl",
      "org.http4s" %% "http4s-ember-server"
    )(http4s)
  )

  val Http4sXml = Def.setting(
    depsJVM("org.http4s" %% "http4s-scala-xml")(http4sScalaXml)
  )

  val JavaTimeJs = Def.setting(
    deps("io.github.cquiroz" %%% "scala-java-time")(javaTimeJs)
  )

  val JuliSlf4j = Def.setting(
    depsJVM("org.slf4j" % "jul-to-slf4j")(slf4j)
  )

  val JwtCore = Def.setting(
    depsJVM("com.github.jwt-scala" %% "jwt-core")(jwt)
  )

  val JwtCirce = Def.setting(
    depsJVM("com.github.jwt-scala" %% "jwt-circe")(jwt)
  )

  val Kittens = Def.setting(
    deps("org.typelevel" %%% "kittens")(kittens)
  )

  val Log4Cats = Def.setting(
    deps("org.typelevel" %%% "log4cats-core")(log4Cats)
  )

  val Log4CatsLogLevel = Def.setting(
    deps("com.rpiaggio" %%% "log4cats-loglevel")(log4CatsLogLevel)
  )

  val Log4CatsNoop = Def.setting(
    deps("org.typelevel" %%% "log4cats-noop")(log4Cats)
  )

  val Log4s = Def.setting(
    deps("org.log4s" %%% "log4s")(log4s)
  )

  val Logback = Def.setting(
    depsJVM(
      "ch.qos.logback" % "logback-core",
      "ch.qos.logback" % "logback-classic"
    )(logback)
  )

  val LucumaAgs = Def.setting(
    deps("edu.gemini" %%% "lucuma-ags")(lucumaCore)
  )

  val LucumaCatalog = Def.setting(
    deps("edu.gemini" %%% "lucuma-catalog")(lucumaCore)
  )

  val LucumaCatalogTestkit = Def.setting(
    deps("edu.gemini" %%% "lucuma-catalog-testkit")(lucumaCore)
  )

  val LucumaCore = Def.setting(
    deps("edu.gemini" %%% "lucuma-core")(lucumaCore)
  )

  val LucumaCoreTestkit = Def.setting(
    deps("edu.gemini" %%% "lucuma-core-testkit")(lucumaCore)
  )

  val LucumaHorizons = Def.setting(
    deps("edu.gemini" %%% "lucuma-horizons")(lucumaCore)
  )

  val LucumaOdbSchema = Def.setting(
    deps("edu.gemini" %%% "lucuma-odb-schema")(lucumaServers)
  )

  val LucumaPrimeStyles = Def.setting(
    deps("edu.gemini" %%% "lucuma-prime-styles")(lucumaPrimeStyles)
  )

  val LucumaReact = Def.setting(
    deps(
      "edu.gemini" %%% "lucuma-react-common",
      "edu.gemini" %%% "lucuma-react-tanstack-table",
      "edu.gemini" %%% "lucuma-react-beautiful-dnd",
      "edu.gemini" %%% "lucuma-react-pragmatic-dnd",
      "edu.gemini" %%% "lucuma-react-circular-progressbar",
      "edu.gemini" %%% "lucuma-react-datepicker",
      "edu.gemini" %%% "lucuma-react-draggable",
      "edu.gemini" %%% "lucuma-react-font-awesome",
      "edu.gemini" %%% "lucuma-react-floatingui",
      "edu.gemini" %%% "lucuma-react-grid-layout",
      "edu.gemini" %%% "lucuma-react-highcharts",
      "edu.gemini" %%% "lucuma-react-hotkeys-hooks",
      "edu.gemini" %%% "lucuma-react-markdown",
      "edu.gemini" %%% "lucuma-react-resize-detector",
      "edu.gemini" %%% "lucuma-react-prime-react"
    )(lucumaReact)
  )

  val LucumaRefined = Def.setting(
    deps("edu.gemini" %%% "lucuma-refined")(lucumaRefined)
  )

  val LucumaSsoFrontendClient = Def.setting(
    deps("edu.gemini" %%% "lucuma-sso-frontend-client")(lucumaServers)
  )

  val LucumaSsoBackendClient = Def.setting(
    deps("edu.gemini" %%% "lucuma-sso-backend-client")(lucumaServers)
  )

  val LucumaItcClient = Def.setting(
    deps("edu.gemini" %%% "lucuma-itc-client")(lucumaServers)
  )

  val Monocle = Def.setting(
    deps(
      "dev.optics" %%% "monocle-core",
      "dev.optics" %%% "monocle-macro",
      "dev.optics" %%% "monocle-unsafe"
    )(monocle)
  )

  val MonocleLaw = Def.setting(
    deps("dev.optics" %%% "monocle-law")(monocle)
  )

  val Mouse = Def.setting(
    deps("org.typelevel" %%% "mouse")(mouse)
  )

  val MUnit = Def.setting(
    deps("org.scalameta" %%% "munit")(mUnit)
  )

  val MUnitScalaCheck = Def.setting(
    deps("org.scalameta" %%% "munit-scalacheck")(mUnitScalacheck)
  )

  val MUnitCatsEffect = Def.setting(
    deps("org.typelevel" %%% "munit-cats-effect")(mUnitCatsEffect)
  )

  val Natchez = Def.setting(
    depsJVM(
      "org.tpolecat" %% "natchez-core",
      "org.tpolecat" %% "natchez-honeycomb"
    )(natchez) ++
      depsJVM("org.tpolecat" %% "natchez-http4s")(natchezHttp4s)
  )

  val PPrint = Def.setting(
    deps("com.lihaoyi" %%% "pprint")(pprint)
  )

  val PureConfig = Def.setting(
    depsJVM(
      "com.github.pureconfig" %% "pureconfig-core",
      "com.github.pureconfig" %% "pureconfig-cats",
      "com.github.pureconfig" %% "pureconfig-cats-effect",
      "com.github.pureconfig" %% "pureconfig-http4s",
      "com.github.pureconfig" %% "pureconfig-ip4s"
    )(pureConfig)
  )

  val ScalaCollectionContrib = Def.setting(
    deps("org.scala-lang.modules" %%% "scala-collection-contrib")(scalaCollectionContrib)
  )

  val ScalaJSDom = Def.setting(
    deps("org.scala-js" %%% "scalajs-dom")(scalaJsDom)
  )

  val ScalaJsReact = Def.setting(
    deps(
      "com.github.japgolly.scalajs-react" %%% "core-bundle-cb_io",
      "com.github.japgolly.scalajs-react" %%% "extra",
      "com.github.japgolly.scalajs-react" %%% "extra-ext-monocle3",
      "com.github.japgolly.scalajs-react" %%% "callback-ext-cats_effect"
    )(scalaJsReact)
  )

  val ScalaJsReactTest = Def.setting(
    deps("com.github.japgolly.scalajs-react" %%% "test")(scalaJsReact)
  )

  val Slf4j = Def.setting(
    depsJVM("org.slf4j" % "slf4j-api")(slf4j)
  )

  val UnboundId = Def.setting(
    depsJVM("com.unboundid" % "unboundid-ldapsdk-minimal-edition")(unboundId)
  )
}
