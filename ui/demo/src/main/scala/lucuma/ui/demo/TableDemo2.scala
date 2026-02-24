// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.demo

import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.*
import lucuma.react.common.style.Css
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.ui.table.hooks.*

import scala.util.Random

object TableDemo2:
  case class Person(id: Int, first: String, last: String, age: Int)
  object Person {
    given Reusability[Person] = Reusability.by_==

    // format: off
    private val adjs = List("autumn", "hidden", "bitter", "misty", "silent",
      "empty", "dry", "dark", "summer", "icy", "delicate", "quiet", "white", "cool",
      "spring", "winter", "patient", "twilight", "dawn", "crimson", "wispy",
      "weathered", "blue", "billowing", "broken", "cold", "damp", "falling",
      "frosty", "green", "long", "late", "lingering", "bold", "little", "morning",
      "muddy", "old", "red", "rough", "still", "small", "sparkling", "throbbing",
      "shy", "wandering", "withered", "wild", "black", "holy", "solitary",
      "fragrant", "aged", "snowy", "proud", "floral", "restless", "divine",
      "polished", "purple", "lively", "nameless", "puffy", "fluffy",
      "calm", "young", "golden", "avenging", "ancestral", "ancient", "argent",
      "reckless", "daunting", "short", "rising", "strong", "timber", "tumbling",
      "silver", "dusty", "celestial", "cosmic", "crescent", "double", "far", "half",
      "inner", "milky", "northern", "southern", "eastern", "western", "outer",
      "terrestrial", "huge", "deep", "epic", "titanic", "mighty", "powerful")

    private val nouns = List("waterfall", "river", "breeze", "moon", "rain",
      "wind", "sea", "morning", "snow", "lake", "sunset", "pine", "shadow", "leaf",
      "dawn", "glitter", "forest", "hill", "cloud", "meadow", "glade",
      "bird", "brook", "butterfly", "bush", "dew", "dust", "field",
      "flower", "firefly", "feather", "grass", "haze", "mountain", "night", "pond",
      "darkness", "snowflake", "silence", "sound", "sky", "shape", "surf",
      "thunder", "violet", "wildflower", "wave", "water", "resonance",
      "sun", "wood", "dream", "cherry", "tree", "fog", "frost", "voice", "paper",
      "frog", "smoke", "star", "sierra", "castle", "fortress", "tiger", "day",
      "sequoia", "cedar", "wrath", "blessing", "spirit", "nova", "storm", "burst",
      "protector", "drake", "dragon", "knight", "fire", "king", "jungle", "queen",
      "giant", "elemental", "throne", "game", "weed", "stone", "apogee", "bang",
      "cluster", "corona", "cosmos", "equinox", "horizon", "light", "nebula",
      "solstice", "spectrum", "universe", "magnitude", "parallax")
    // format: on

    def random(id: Int): Person = {
      def getRandElt[A](xs: List[A]): A = xs.apply(Random.nextInt(xs.size))

      def getAge: Int = Random.nextInt(110)

      Person(id, getRandElt(adjs), getRandElt(nouns), getAge)
    }

    def randomPeople(count: Int) = (0 to count).map(random).toList
  }

  private val ColDef = ColumnDef[Person]

  private val columns =
    Reusable.always:
      List(
        ColDef(ColumnId("handle"), cell = _ => <.span(Css("handle"), "≡")).withSize(20.toPx),
        ColDef(ColumnId("first"), _.first, _ => "First", size = 150.toPx),
        ColDef(ColumnId("last"), _.last, _ => "Last", size = 150.toPx),
        ColDef(ColumnId("age"), _.age, _ => "Age", size = 50.toPx)
      )

  val component =
    ScalaFnComponent[Unit]: _ =>
      for
        people   <- useMemo(())(_ => Person.randomPeople(100))
        table    <- useReactTable:
                      TableOptions(columns, people)
        tableDnd <- useVirtualizedTableDragAndDrop(
                      table,
                      ColumnId("handle"),
                      getData = _.original.id,
                      onDrop = (sourceData, target) =>
                        Callback.log:
                          s"Dropped $sourceData on: $target"
                    )
      yield tableDnd.context(
        <.h2("Drag and Drop Virtualized Table"),
        HTMLVirtualizedTable(
          table,
          containerRef = tableDnd.containerRef,
          containerMod = Css("container"),
          estimateSize = _ => 24.toPx,
          rowMod = tableDnd.rowMod(),
          cellMod = tableDnd.cellMod()
        )
      )
