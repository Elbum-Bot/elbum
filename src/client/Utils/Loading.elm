module Utils.Loading exposing (LoadState(..), LoadingMsg, ManyModel, ManyMsg, cmdFor, cmdForMany, getOneState, getState, init, initMany, mark, markOne, subscriptions, subscriptionsMany, update, updateMany)

import Dict exposing (Dict, filter, fromList, get, insert, size, toList, values)
import Http exposing (Error, Progress, emptyBody, expectWhatever, track)
import List exposing (head, length, tail)
import Task
import Url exposing (Url, fromString, toString)


type LoadState
    = NotRequested
    | RequestedButNoProgress
    | Loading Progress
    | Loaded
    | Marked LoadState
    | Failed Error


type LoadingMsg
    = Requested
    | GotProgress Progress
    | Finished
    | Failure Error


type LoadingModel
    = LoadingModel
        { url : Url
        , state : LoadState
        , tracker : String
        }


type OneModel msg
    = OneModel LoadingModel (LoadingMsg -> msg)


type ManyModel msg
    = ManyModel
        { pending : List Url
        , maxConcurrentCount : Int
        , models : Dict String LoadingModel
        , wrap : ManyMsg -> msg
        }


type ManyMsg
    = ManyMsg Url LoadingMsg


init : (LoadingMsg -> msg) -> Url -> ( OneModel msg, Cmd msg )
init wrap url =
    let
        tracker =
            "LoadingTracker:" ++ toString url

        model =
            OneModel
                (LoadingModel
                    { url = url
                    , state = NotRequested
                    , tracker = tracker
                    }
                )
                wrap
    in
    ( model
    , cmdFor model
    )


initMany : List Url -> List Url -> (ManyMsg -> msg) -> ( ManyModel msg, Cmd msg )
initMany firstUrls restUrls wrap =
    let
        initEntry url =
            ( toString url, init (ManyMsg url >> wrap) url )

        models =
            fromList <| List.map initEntry firstUrls
    in
    ( ManyModel
        { pending = List.filter (\u -> not <| List.member u firstUrls) restUrls
        , maxConcurrentCount = length firstUrls
        , models = Dict.map (always <| Tuple.first >> getModel) models
        , wrap = wrap
        }
    , Cmd.batch <| values <| Dict.map (always Tuple.second) models
    )


update : LoadingMsg -> OneModel msg -> OneModel msg
update msg (OneModel (LoadingModel m) wrap) =
    case msg of
        Requested ->
            case m.state of
                NotRequested ->
                    OneModel (LoadingModel { m | state = RequestedButNoProgress }) wrap

                _ ->
                    OneModel (LoadingModel m) wrap

        GotProgress progress ->
            let
                updatedWithProgress =
                    OneModel (LoadingModel { m | state = Loading progress }) wrap

                ignoreStaleProgress =
                    OneModel (LoadingModel m) wrap

                progressHandlerFor state =
                    case state of
                        NotRequested ->
                            updatedWithProgress

                        RequestedButNoProgress ->
                            updatedWithProgress

                        Loading _ ->
                            updatedWithProgress

                        Loaded ->
                            ignoreStaleProgress

                        Failed _ ->
                            ignoreStaleProgress

                        Marked s ->
                            progressHandlerFor s
            in
            progressHandlerFor m.state

        Finished ->
            OneModel (LoadingModel { m | state = Loaded }) wrap

        Failure error ->
            OneModel (LoadingModel { m | state = Failed error }) wrap


updateMany : ManyMsg -> ManyModel msg -> (List Url -> List Url) -> ( ManyModel msg, Cmd msg )
updateMany (ManyMsg url loadingMsg) (ManyModel mm) revisePending =
    case get (toString url) mm.models of
        Just oneModel ->
            let
                (OneModel (LoadingModel oneNewModel) wrap) =
                    update loadingMsg (OneModel oneModel (ManyMsg url >> mm.wrap))

                oneNewCmd =
                    cmdFor <| OneModel (LoadingModel oneNewModel) wrap

                oneNewModels =
                    insert (toString url) (LoadingModel oneNewModel) mm.models

                isNewUrl u =
                    (u /= url) && (not <| List.member u (List.map (\(LoadingModel lm) -> lm.url) <| Dict.values oneNewModels))

                revisedPending =
                    List.filter isNewUrl <| revisePending <| List.filter isNewUrl mm.pending

                ( allNewModels, newPending, newCmds ) =
                    promotePending mm.wrap mm.maxConcurrentCount oneNewModels revisedPending
            in
            ( ManyModel
                { pending = newPending
                , maxConcurrentCount = mm.maxConcurrentCount
                , models = allNewModels
                , wrap = mm.wrap
                }
            , Cmd.batch [ newCmds, oneNewCmd ]
            )

        Nothing ->
            ( ManyModel mm
            , Cmd.none
            )


isLoading (LoadingModel m) =
    let
        isLoadingImpl state =
            case state of
                NotRequested ->
                    True

                RequestedButNoProgress ->
                    True

                Loading _ ->
                    True

                Loaded ->
                    False

                Failed _ ->
                    False

                Marked s ->
                    isLoadingImpl s
    in
    isLoadingImpl m.state


promotePending : (ManyMsg -> msg) -> Int -> Dict String LoadingModel -> List Url -> ( Dict String LoadingModel, List Url, Cmd msg )
promotePending wrap maxConcurrentCount currentModels pending =
    let
        inProgCount =
            size <| filter (always isLoading) currentModels
    in
    case inProgCount >= maxConcurrentCount of
        True ->
            ( currentModels, pending, Cmd.none )

        False ->
            case head pending of
                Nothing ->
                    ( currentModels, pending, Cmd.none )

                Just nextUrl ->
                    let
                        ( OneModel oneNewModel _, oneNewCmd ) =
                            init (wrap << ManyMsg nextUrl) nextUrl
                    in
                    ( insert (toString nextUrl) oneNewModel currentModels
                    , Maybe.withDefault [] <| tail pending
                    , oneNewCmd
                    )


subscriptions : OneModel msg -> Sub msg
subscriptions (OneModel (LoadingModel lm) wrap) =
    let
        trackIt =
            track lm.tracker <| wrap << GotProgress

        ignoreIt =
            Sub.none

        subForState state =
            case state of
                NotRequested ->
                    ignoreIt

                RequestedButNoProgress ->
                    trackIt

                Loading _ ->
                    trackIt

                Loaded ->
                    ignoreIt

                Failed _ ->
                    ignoreIt

                Marked s ->
                    subForState s
    in
    subForState lm.state


subscriptionsMany : ManyModel msg -> Sub msg
subscriptionsMany (ManyModel mm) =
    let
        subForOneModel ( oneUrlStr, LoadingModel m ) =
            case fromString oneUrlStr of
                Nothing ->
                    []

                Just oneUrl ->
                    [ subscriptions <| OneModel (LoadingModel m) <| ManyMsg oneUrl >> mm.wrap ]
    in
    mm.models |> toList |> List.concatMap subForOneModel |> Sub.batch


cmdFor : OneModel msg -> Cmd msg
cmdFor (OneModel (LoadingModel m) wrap) =
    let
        handle : Result Error () -> LoadingMsg
        handle result =
            case result of
                Ok _ ->
                    Finished

                Err err ->
                    Failure err

        get =
            Http.request
                { method = "GET"
                , headers = []
                , url = toString m.url
                , body = emptyBody
                , expect = expectWhatever handle
                , timeout = Nothing
                , tracker = Just m.tracker
                }

        requested =
            Task.perform identity <| Task.succeed Requested

        cmdForImpl state =
            case state of
                NotRequested ->
                    Cmd.batch [ get, requested ]

                RequestedButNoProgress ->
                    Cmd.none

                Loading _ ->
                    Cmd.none

                Loaded ->
                    Cmd.none

                Failed _ ->
                    Cmd.none

                Marked s ->
                    cmdForImpl s
    in
    Cmd.map wrap <| cmdForImpl m.state


cmdForMany : ManyModel msg -> Cmd msg
cmdForMany (ManyModel mm) =
    let
        cmdForOneModel ( oneUrlStr, LoadingModel m ) =
            case fromString oneUrlStr of
                Nothing ->
                    []

                Just oneUrl ->
                    [ cmdFor <| OneModel (LoadingModel m) <| mm.wrap << ManyMsg oneUrl ]
    in
    Cmd.batch <| List.concatMap cmdForOneModel <| Dict.toList mm.models


getState : OneModel msg -> LoadState
getState (OneModel (LoadingModel m) _) =
    m.state


getOneState : ManyModel msg -> Url -> Maybe LoadState
getOneState (ManyModel mm) url =
    case get (toString url) mm.models of
        Just (LoadingModel m) ->
            Just m.state

        Nothing ->
            case List.member url mm.pending of
                True ->
                    Just NotRequested

                False ->
                    Nothing


getModel : OneModel msg -> LoadingModel
getModel (OneModel m _) =
    m


mark : OneModel msg -> OneModel msg
mark (OneModel (LoadingModel m) wrap) =
    case m.state of
        Marked _ ->
            OneModel (LoadingModel m) wrap

        _ ->
            OneModel (LoadingModel { m | state = Marked m.state }) wrap


markOne : ManyModel msg -> Url -> ManyModel msg
markOne (ManyModel mm) url =
    case get (toString url) mm.models of
        Nothing ->
            ManyModel mm

        Just (LoadingModel m) ->
            case m.state of
                Marked _ ->
                    ManyModel mm

                _ ->
                    ManyModel { mm | models = insert (toString url) (LoadingModel { m | state = Marked m.state }) mm.models }