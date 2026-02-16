// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Endo
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import clue.data.syntax.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.AladinFullScreen
import explore.model.AppContext
import explore.model.AttachmentList
import explore.model.BlindOffset
import explore.model.ExploreModelValidators
import explore.model.GuideStarSelection
import explore.model.ObsConfiguration
import explore.model.ObsIdSet
import explore.model.ObservationTargets
import explore.model.ObservationsAndTargets
import explore.model.OnCloneParameters
import explore.model.TargetEditObsInfo
import explore.model.UserPreferences
import explore.model.reusability.given
import explore.services.OdbAsterismApi
import explore.services.OdbTargetApi
import explore.syntax.ui.*
import explore.targeteditor.RVInput
import explore.targets.TargetSource
import explore.undo.UndoSetter
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ProgramType
import lucuma.core.enums.TargetDisposition
import lucuma.core.math.*
import lucuma.core.math.validation.MathValidators
import lucuma.core.model.CatalogInfo
import lucuma.core.model.Ephemeris
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.react.common.*
import lucuma.react.primereact.Message
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.TargetWithId
import lucuma.schemas.model.enums.BlindOffsetType
import lucuma.schemas.odb.input.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.FormInputText
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.LucumaPrimeStyles
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import monocle.Prism
import org.typelevel.log4cats.Logger

import java.time.Instant

case class TargetEditor(
  programId:           Program.Id,
  programType:         ProgramType,
  userId:              User.Id,
  targetWithId:        UndoSetter[TargetWithId],
  obsAndTargets:       UndoSetter[ObservationsAndTargets],
  // TODO, we may derive obsTargets from obsAndTargets
  obsTargets:          ObservationTargets, // This is passed through to Aladin, to plot the entire ObservationTargets.
  obsTime:             Option[Instant],
  obsConf:             Option[ObsConfiguration],
  searching:           View[Set[Target.Id]],
  obsInfo:             TargetEditObsInfo,
  onClone:             OnCloneParameters => Callback,
  fullScreen:          View[AladinFullScreen],
  userPreferences:     View[UserPreferences],
  guideStarSelection:  View[GuideStarSelection],
  attachments:         View[AttachmentList],
  authToken:           Option[NonEmptyString],
  readonly:            Boolean,
  allowEditingOngoing: Boolean,
  isStaffOrAdmin:      Boolean,
  invalidateSequence:  Callback = Callback.empty,
  blindOffsetInfo:     Option[(Observation.Id, View[BlindOffset])] = none
) extends ReactFnProps(TargetEditor.component):
  def toManualBlindOffset: Callback =
    if targetWithId.get.disposition === TargetDisposition.BlindOffset then
      blindOffsetInfo.foldMap: (_, bo) =>
        bo.zoom(BlindOffset.blindOffsetType).set(BlindOffsetType.Manual)
    else Callback.empty

object TargetEditor:
  private type Props = TargetEditor

  private def cloneTarget(
    programId:     Program.Id,
    targetId:      Target.Id,
    obsIds:        ObsIdSet,
    cloning:       View[Boolean],
    obsAndTargets: UndoSetter[ObservationsAndTargets],
    onClone:       OnCloneParameters => Callback
  )(
    input:         UpdateTargetsInput
  )(using
    odbApi:        OdbTargetApi[IO] & OdbAsterismApi[IO]
  )(using Logger[IO], ToastCtx[IO]): IO[Unit] =
    odbApi
      .cloneTarget(targetId, obsIds, input)
      .flatMap: clone =>
        (TargetCloneAction
          .cloneTarget(programId, targetId, clone, obsIds, onClone)
          .set(obsAndTargets)(clone.target.some) >>
          // If we do the first `onClone` here, the UI works correctly.
          onClone(OnCloneParameters(targetId, clone.id, obsIds, true))).toAsync
      .switching(cloning.async)
      .handleErrorWith: t => // TODO Move error handling to API layer
        val msg = s"Error cloning target [$targetId]"
        Logger[IO].error(t)(msg) >>
          ToastCtx[IO].showToast(msg, Message.Severity.Error)

  // An UndoSetter that doesn't really update any undo stacks
  private def noopUndoSetter[M](view: View[M]): UndoSetter[M] =
    new UndoSetter[M] {
      val model = view
      def set[A](
        getter:    M => A,
        setter:    A => M => M,
        onSet:     (M, A) => IO[Unit],
        onRestore: (M, A) => IO[Unit]
      )(v: A): Callback =
        mod(getter, setter, onSet, onRestore)(_ => v)

      def mod[A](
        getter:    M => A,
        setter:    A => M => M,
        onSet:     (M, A) => IO[Unit],
        onRestore: (M, A) => IO[Unit]
      )(f: A => A): Callback =
        model.modCB(
          oldModel => setter(f(getter(oldModel)))(oldModel),
          (oldModel, newModel) => onSet(oldModel, getter(newModel)).runAsyncAndForget
        )
    }

  private def emptySourceProfileInput(sp: SourceProfile): SourceProfileInput =
    sp match
      case SourceProfile.Point(_)       =>
        SourceProfileInput.Point:
          SpectralDefinitionIntegratedInput.BandNormalized(BandNormalizedIntegratedInput())
      case SourceProfile.Uniform(_)     =>
        SourceProfileInput.Uniform:
          SpectralDefinitionSurfaceInput.BandNormalized(BandNormalizedSurfaceInput())
      case SourceProfile.Gaussian(_, _) =>
        SourceProfileInput.Gaussian:
          GaussianInput()

  private val component =
    ScalaFnComponent[Props]: props =>
      for
        ctx                 <- useContext(AppContext.ctx)
        cloning             <- useStateView(false)
        obsToCloneTo        <- useStateView(none[ObsIdSet]) // obs ids to clone to.
        // flag for readonly based on the execution status of the observation(s)
        readonlyForStatuses <- useStateView(false)
        // If obsTime is not set, change it to now
        obsTime             <- useEffectKeepResultWithDeps(props.obsTime): obsTime =>
                                 IO(obsTime.getOrElse(Instant.now()))
        // select the aligner to use based on whether a clone will be created or not.
        targetAligner       <-
          useMemo(
            (props.programId,
             props.targetWithId.get.target,
             props.targetWithId.get.id,
             obsToCloneTo.get
            )
          ): (pid, target, tid, toCloneTo) =>
            import ctx.given

            toCloneTo.fold(
              Aligner(
                props.targetWithId.zoom(TargetWithId.target),
                UpdateTargetsInput(
                  WHERE = tid.toWhereTarget.assign,
                  SET = TargetPropertiesInput()
                ),
                // Invalidate the sequence if the target changes, and if it is a blind offset
                // make it a manual blind offset
                u =>
                  props.invalidateSequence.to[IO] >>
                    props.toManualBlindOffset.to[IO] >> ctx.odbApi.updateTarget(u)
              )
            ): obsIds =>
              val view = View(target, (mod, cb) => cb(target, mod(target)))
              Aligner(
                noopUndoSetter(view),
                // noopUndoSetter(noUndoTargetView),
                UpdateTargetsInput(SET = TargetPropertiesInput()),
                u =>
                  props.invalidateSequence.to[IO] *>
                    cloneTarget(
                      pid,
                      tid,
                      obsIds,
                      cloning,
                      props.obsAndTargets,
                      props.onClone
                    )(u)
              )
      yield
        import ctx.given

        val disabled: Boolean =
          props.searching.get.exists(_ === props.obsTargets.focus.id) ||
            cloning.get || props.readonly || readonlyForStatuses.get ||
            props.targetWithId.get.isReadonlyForProgramType(props.programType)

        val catalogInfo: Option[CatalogInfo] =
          Target.catalogInfo.getOption(props.targetWithId.get.target).flatten

        val nameLens          = UpdateTargetsInput.SET.andThen(TargetPropertiesInput.name)
        val siderealLens      = UpdateTargetsInput.SET.andThen(TargetPropertiesInput.sidereal)
        val nonsideralLens    = UpdateTargetsInput.SET.andThen(TargetPropertiesInput.nonsidereal)
        val opportunityLens   = UpdateTargetsInput.SET.andThen(TargetPropertiesInput.opportunity)
        val sourceProfileLens = UpdateTargetsInput.SET.andThen(TargetPropertiesInput.sourceProfile)

        extension [A, B](prism: Prism[A, B])
          def optReplace[I](a: A, f: B => (I => I)): I => I =
            i => prism.getOption(a).fold(i)(b => f(b)(i))

        extension (region: Region)
          def toOpportunityInput: OpportunityInput =
            OpportunityInput(
              region = RegionInput(
                rightAscensionArc = RightAscensionArcInput(
                  `type` = region.raArc.arcType,
                  start = Arc.start.getOption(region.raArc).map(_.toInput).orUnassign,
                  end = Arc.end.getOption(region.raArc).map(_.toInput).orUnassign
                ),
                declinationArc = DeclinationArcInput(
                  `type` = region.decArc.arcType,
                  start = Arc.start.getOption(region.decArc).map(_.toInput).orUnassign,
                  end = Arc.end.getOption(region.decArc).map(_.toInput).orUnassign
                )
              )
            )

        val allView: View[Target] =
          targetAligner.viewMod(t =>
            nameLens.replace(t.name.assign) >>>
              Target.sidereal.optReplace(t, s => siderealLens.replace(s.toInput.assign)) >>>
              Target.nonsidereal.optReplace(t, ns => nonsideralLens.replace(ns.toInput.assign)) >>>
              Target.opportunity.optReplace(t, o => opportunityLens.replace(o.toInput.assign)) >>>
              sourceProfileLens.replace(t.sourceProfile.toInput.assign)
          )

        val siderealToTargetEndo: Endo[SiderealInput] => Endo[UpdateTargetsInput] =
          forceAssign(siderealLens.modify)(SiderealInput())

        val optSiderealAligner: Option[Aligner[Target.Sidereal, SiderealInput]] =
          targetAligner.value.zoomOpt(
            Target.sidereal,
            siderealToTargetEndo
          )

        val optOpportunityAligner: Option[Aligner[Target.Opportunity, TargetPropertiesInput]] =
          targetAligner.value.zoomOpt(
            Target.opportunity,
            UpdateTargetsInput.SET.modify
          )

        val nameView: View[NonEmptyString] =
          targetAligner
            .zoom(Target.name, nameLens.modify)
            .view(_.assign)

        val sourceProfileAligner: Aligner[SourceProfile, SourceProfileInput] =
          targetAligner.zoom(
            Target.sourceProfile,
            forceAssign(sourceProfileLens.modify)(
              emptySourceProfileInput(targetAligner.get.sourceProfile)
            )
          )

        def siderealCoordinates(
          siderealTargetAligner: Aligner[Target.Sidereal, SiderealInput]
        ): VdomElement = {

          val coordsRAView: View[RightAscension] =
            siderealTargetAligner
              .zoom(Target.Sidereal.baseRA, SiderealInput.ra.modify)
              .view(_.toInput.assign)

          val coordsDecView: View[Declination] =
            siderealTargetAligner
              .zoom(Target.Sidereal.baseDec, SiderealInput.dec.modify)
              .view(_.toInput.assign)

          React.Fragment(
            FormInputTextView(
              id = "ra".refined,
              value = coordsRAView,
              label = React.Fragment("RA", HelpIcon("target/main/coordinates.md".refined)),
              disabled = disabled,
              validFormat = MathValidators.truncatedRA,
              changeAuditor = ChangeAuditor.truncatedRA
            ),
            FormInputTextView(
              id = "dec".refined,
              value = coordsDecView,
              label = React.Fragment("Dec", HelpIcon("target/main/coordinates.md".refined)),
              disabled = disabled,
              validFormat = MathValidators.truncatedDec,
              changeAuditor = ChangeAuditor.truncatedDec
            )
          )
        }

        def opportunityRegion(
          opportunityAligner: Aligner[Target.Opportunity, TargetPropertiesInput]
        ): VdomElement =
          val regionView: View[Region] =
            opportunityAligner
              .zoom(Target.Opportunity.region, TargetPropertiesInput.opportunity.modify)
              .view(_.toOpportunityInput.assign)
          RegionEditor(regionView, disabled)

        val ephemerisKey: Option[VdomNode] =
          Target.nonsidereal
            .getOption(targetAligner.get)
            .map(_.ephemerisKey)
            .map: key =>
              val (label, value) = key match
                case Ephemeris.Key.UserSupplied(id) =>
                  ("User Supplied", id.toString)
                case h: Ephemeris.Key.Horizons      =>
                  ("Horizons", s"${key.keyType.simplifiedName} ${h.des}")
              FormInputText(
                id = "ephemeris-key".refined,
                value = value,
                label = React.Fragment(
                  label,
                  HelpIcon("target/main/ephemeris-key.md".refined)
                ),
                disabled = true
              )

        def siderealTracking(
          siderealTargetAligner: Aligner[Target.Sidereal, SiderealInput]
        ): VdomElement = {
          val epochView: View[Epoch] =
            siderealTargetAligner
              .zoom(Target.Sidereal.epoch, SiderealInput.epoch.modify)
              .view(Epoch.fromString.reverseGet.andThen(_.assign))

          val properMotionView: View[ProperMotion] =
            siderealTargetAligner
              .zoom(Target.Sidereal.properMotion, SiderealInput.properMotion.modify)
              .view(_.map(_.toInput).orUnassign)
              .removeOptionality(ProperMotion.Zero)

          val properMotionRAView: View[ProperMotion.RA] =
            properMotionView.zoom(ProperMotion.ra)

          val properMotionDecView: View[ProperMotion.Dec] =
            properMotionView.zoom(ProperMotion.dec)

          val parallaxView: View[Parallax] =
            siderealTargetAligner
              .zoom(
                Target.Sidereal.parallax,
                SiderealInput.parallax.modify
              )
              .view(_.map(_.toInput).orUnassign)
              .removeOptionality(Parallax.Zero)

          val radialVelocityView: View[RadialVelocity] =
            siderealTargetAligner
              .zoom(
                Target.Sidereal.radialVelocity,
                SiderealInput.radialVelocity.modify
              )
              .view(_.map(_.toInput).orUnassign)
              .removeOptionality(RadialVelocity.Zero)

          <.div(
            LucumaPrimeStyles.FormColumnVeryCompact,
            ExploreStyles.TargetProperMotionForm,
            FormInputTextView(
              id = "epoch".refined,
              value = epochView,
              label = React.Fragment("Epoch", HelpIcon("target/main/epoch.md".refined)),
              disabled = disabled,
              validFormat = MathValidators.epochNoScheme,
              changeAuditor = ChangeAuditor.maxLength(8.refined).decimal(3.refined).denyNeg,
              units = "years"
            ),
            FormInputTextView(
              id = "raPM".refined,
              value = properMotionRAView,
              label = "µ RA",
              disabled = disabled,
              validFormat = ExploreModelValidators.pmRAValidWedge,
              changeAuditor = ChangeAuditor.bigDecimal(3.refined),
              units = "mas/y",
              groupClass = ExploreStyles.ZeroValue.when_(
                properMotionRAView.get === ProperMotion.Zero.ra
              )
            ),
            FormInputTextView(
              id = "raDec".refined,
              value = properMotionDecView,
              label = "µ Dec",
              disabled = disabled,
              validFormat = ExploreModelValidators.pmDecValidWedge,
              changeAuditor = ChangeAuditor.bigDecimal(3.refined),
              units = "mas/y",
              groupClass = ExploreStyles.ZeroValue.when_(
                properMotionDecView.get === ProperMotion.Zero.dec
              )
            ),
            FormInputTextView(
              id = "parallax".refined,
              value = parallaxView,
              label = "Parallax",
              disabled = disabled,
              validFormat = ExploreModelValidators.pxValidWedge,
              changeAuditor = ChangeAuditor.bigDecimal(3.refined),
              units = "mas",
              groupClass = ExploreStyles.ZeroValue.when_(
                parallaxView.get === Parallax.Zero
              )
            ),
            RVInput(
              radialVelocityView,
              disabled,
              props.obsConf.flatMap(_.calibrationRole),
              props.obsTargets.focus.id,
              props.userPreferences,
              props.userId
            )
          )
        }

        val targetSources: NonEmptyList[TargetSource[IO]] =
          props.obsTargets.focus.target match
            case Target.Nonsidereal(_, _, _) =>
              NonEmptyList.one(TargetSource.FromHorizons[IO](ctx.horizonsClient))
            case _                           =>
              NonEmptyList.one(TargetSource.FromSimbad[IO](ctx.simbadClient))

        React.Fragment(
          TargetCloneSelector(
            props.obsInfo,
            obsToCloneTo,
            readonlyForStatuses,
            props.allowEditingOngoing
          ),
          <.div(ExploreStyles.TargetGrid)(
            // If there is a ToO in the obsTargets, we won't have a baseTracking and will skip visualization.
            obsTime.value.renderPot(ot =>
              AladinCell(
                props.userId,
                props.obsTargets,
                ot,
                props.obsConf,
                props.fullScreen,
                props.userPreferences,
                props.guideStarSelection,
                props.blindOffsetInfo,
                props.obsAndTargets.model.zoom(ObservationsAndTargets.targets),
                props.isStaffOrAdmin,
                props.readonly
              )
            ),
            <.div(LucumaPrimeStyles.FormColumnVeryCompact, ExploreStyles.TargetForm)(
              // Keep the search field and the coords always together
              SearchForm(
                props.obsTargets.focus.id,
                // SearchForm doesn't edit the name directly. It will set it atomically, together
                // with coords & magnitudes from the catalog search, so that all 3 fields are
                // a single undo/redo operation.
                nameView,
                targetSources,
                allView.set,
                props.searching,
                disabled,
                cloning.get,
                disableSearch =
                  props.targetWithId.get.disposition === TargetDisposition.BlindOffset ||
                    props.targetWithId.get.isTargetOfOpportunity
              ),
              optSiderealAligner.map(siderealCoordinates),
              optOpportunityAligner.map(opportunityRegion),
              ephemerisKey
            ),
            optSiderealAligner.map(siderealTracking),
            <.div(
              ExploreStyles.Grid,
              ExploreStyles.Compact,
              LucumaPrimeStyles.FormColumnVeryCompact,
              ExploreStyles.TargetSourceProfileEditor,
              ExploreStyles.WithGaussian
                .when(SourceProfile.gaussian.getOption(sourceProfileAligner.get).isDefined),
              ExploreStyles.WithCatalogInfo
                .when(catalogInfo.flatMap(_.objectType).isDefined)
            )(
              // The `withKey` is important because React wasn't updating the BrightnessesEditor
              // or the EmissionsLineEditor when the obsIdSubset changed, resulting in targets always
              // being cloned even when all targets should have been edited.
              SourceProfileEditor(
                props.programId,
                sourceProfileAligner,
                catalogInfo,
                props.attachments,
                props.authToken,
                props.obsConf.flatMap(_.calibrationRole),
                disabled,
                props.userPreferences.get.globalPreferences.wavelengthUnits
              ).withKey(obsToCloneTo.get.fold("none")(_.show))
            )
          )
        )
