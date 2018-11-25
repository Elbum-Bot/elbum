module AlbumPage exposing (AlbumPage(..), AlbumPageMsg(..), ViewportInfo, progInit, resetUrls, subscriptions, titleOf, update, urlsToGet, view)

import Album exposing (..)
import AlbumStyles exposing (..)
import Browser.Dom exposing (..)
import Browser.Events exposing (..)
import FullImagePage exposing (..)
import Html.Styled exposing (..)
import ImageViews exposing (..)
import Json.Decode exposing (..)
import KeyboardUtils exposing (onEscape)
import ListUtils exposing (..)
import ProgressiveImage exposing (..)
import ResultUtils exposing (..)
import Set exposing (..)
import Task exposing (..)
import ThumbPage exposing (..)
import TouchEvents exposing (..)


type AlbumPage
    = Thumbs Album ViewportInfo (Set String) (Set String)
    | FullImage (List Image) Album ProgressiveImageModel ViewportInfo (Maybe Float) (Maybe ( Touch, Touch ))


type alias ViewportInfo =
    { bodyViewport : Viewport, rootDivViewport : Maybe Viewport }


type AlbumPageMsg
    = View (List Image) Image (List Image)
    | TouchDragStart Touch
    | TouchDragContinue Touch
    | TouchDragAbandon
    | Prev
    | Next
    | BackToThumbs
    | FullMsg ProgressiveImageMsg
    | NoUpdate


update : AlbumPageMsg -> AlbumPage -> Maybe Float -> ( AlbumPage, Cmd AlbumPageMsg )
update msg model scroll =
    case msg of
        View prevImgs curImg nextImgs ->
            case model of
                Thumbs album vpInfo _ _ ->
                    let
                        ( w, h ) =
                            fitImage curImg.srcSetFirst (floor vpInfo.bodyViewport.viewport.width) (floor vpInfo.bodyViewport.viewport.height)

                        ( progModel, progCmd ) =
                            progInit vpInfo.bodyViewport curImg w h
                    in
                    ( FullImage
                        prevImgs
                        { title = album.title
                        , imageFirst = curImg
                        , imageRest = nextImgs
                        , thumbnail = album.thumbnail
                        }
                        progModel
                        vpInfo
                        scroll
                        Nothing
                    , Cmd.map FullMsg <| Maybe.withDefault Cmd.none <| Maybe.map toCmd progCmd
                    )

                _ ->
                    ( model, Cmd.none )

        Prev ->
            updatePrevNext model shiftLeft

        Next ->
            updatePrevNext model shiftRight

        BackToThumbs ->
            case model of
                FullImage prevImgs album _ viewport savedScroll _ ->
                    let
                        ( newFirst, newRest ) =
                            shiftToBeginning prevImgs album.imageFirst album.imageRest

                        scrollCmd =
                            case savedScroll of
                                Nothing ->
                                    Cmd.none

                                Just pos ->
                                    Task.attempt (\_ -> NoUpdate) <| setViewportOf rootDivId 0 pos
                    in
                    ( Thumbs
                        { title = album.title
                        , imageFirst = newFirst
                        , imageRest = newRest
                        , thumbnail = album.thumbnail
                        }
                        viewport
                        empty
                        empty
                    , scrollCmd
                    )

                _ ->
                    ( model, Cmd.none )

        TouchDragStart pos ->
            case model of
                FullImage prevImgs album progModel viewport savedScroll dragInfo ->
                    ( FullImage prevImgs album progModel viewport savedScroll (Just ( pos, pos )), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TouchDragContinue pos ->
            case model of
                FullImage prevImgs album progModel viewport savedScroll dragInfo ->
                    case dragInfo of
                        Nothing ->
                            ( FullImage prevImgs album progModel viewport savedScroll (Just ( pos, pos )), Cmd.none )

                        Just ( start, cur ) ->
                            ( FullImage prevImgs album progModel viewport savedScroll (Just ( start, pos )), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TouchDragAbandon ->
            case model of
                FullImage prevImgs album progModel viewport savedScroll _ ->
                    ( FullImage prevImgs album progModel viewport savedScroll Nothing, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FullMsg progImgMsg ->
            case model of
                FullImage prevImgs album progModel viewport savedScroll dragInfo ->
                    let
                        ( newProgModel, newProgCmd ) =
                            ProgressiveImage.update progImgMsg progModel
                    in
                    ( FullImage prevImgs album newProgModel viewport savedScroll dragInfo, Cmd.map FullMsg newProgCmd )

                _ ->
                    ( model, Cmd.none )

        NoUpdate ->
            ( model, Cmd.none )


progInit : Viewport -> Image -> Int -> Int -> ( ProgressiveImageModel, Maybe ProgressiveImageMsg )
progInit viewport i w h =
    let
        ( _, thumbWidth ) =
            colsWidth viewport

        smBiggerThan wMax hMax =
            smallestImageBiggerThan wMax hMax i.srcSetFirst i.srcSetRest
    in
    ProgressiveImage.init
        { mainImg = smBiggerThan w h
        , fallback = smBiggerThan 1 1
        , possiblyCached = [ smBiggerThan thumbWidth 1 ]
        , width = w
        , height = h
        }


updatePrevNext : AlbumPage -> (List Image -> Image -> List Image -> ( List Image, Image, List Image )) -> ( AlbumPage, Cmd AlbumPageMsg )
updatePrevNext model shifter =
    case model of
        FullImage prevImgs album oldProgModel vpInfo savedScroll _ ->
            let
                ( newPrev, newCur, newRest ) =
                    shifter prevImgs album.imageFirst album.imageRest

                ( newProgModel, newCmd ) =
                    if album.imageFirst == newCur then
                        ( oldProgModel, Nothing )

                    else
                        let
                            ( w, h ) =
                                fitImage newCur.srcSetFirst (floor vpInfo.bodyViewport.viewport.width) (floor vpInfo.bodyViewport.viewport.height)
                        in
                        progInit vpInfo.bodyViewport newCur w h
            in
            ( FullImage
                newPrev
                { title = album.title
                , imageFirst = newCur
                , imageRest = newRest
                , thumbnail = album.thumbnail
                }
                newProgModel
                vpInfo
                savedScroll
                Nothing
            , Cmd.map FullMsg <| Maybe.withDefault Cmd.none <| Maybe.map toCmd newCmd
            )

        _ ->
            ( model, Cmd.none )


resetUrls : AlbumPageMsg -> Bool
resetUrls msg =
    case msg of
        BackToThumbs ->
            True

        View _ _ _ ->
            True

        _ ->
            False


urlsToGet : AlbumPage -> Set String
urlsToGet albumPage =
    case albumPage of
        Thumbs album vpInfo justLoadedImages readyToDisplayImages ->
            ThumbPage.urlsToGet
                { album = album
                , parents = []
                , bodyViewport = vpInfo.bodyViewport
                , rootDivViewport = vpInfo.rootDivViewport
                , justLoadedImages = justLoadedImages
                , readyToDisplayImages = readyToDisplayImages
                }

        _ ->
            empty


titleOf : AlbumPage -> String
titleOf albumPage =
    case albumPage of
        Thumbs album _ _ _ ->
            album.title

        FullImage _ album _ _ _ _ ->
            album.imageFirst.altText


view : AlbumPage -> (Viewport -> msg) -> (AlbumList -> msg) -> (AlbumPageMsg -> msg) -> List AlbumList -> AlbumBootstrapFlags -> Html msg
view albumPage scrollMsgMaker showList wrapMsg parents flags =
    case albumPage of
        Thumbs album vpInfo justLoadedImages readyToDisplayImages ->
            ThumbPage.view
                scrollMsgMaker
                (\x -> \y -> \z -> wrapMsg (View x y z))
                showList
                { album = album
                , parents = parents
                , bodyViewport = vpInfo.bodyViewport
                , rootDivViewport = vpInfo.rootDivViewport
                , justLoadedImages = justLoadedImages
                , readyToDisplayImages = readyToDisplayImages
                }
                flags

        FullImage prevImgs album progModel vpInfo _ dragInfo ->
            FullImagePage.view
                { prevMsg = wrapMsg Prev
                , nextMsg = wrapMsg Next
                , backToThumbsMsg = wrapMsg BackToThumbs
                , showList = showList
                }
                { touchStartMsg = wrapMsg << TouchDragStart
                , touchContinueMsg = wrapMsg << TouchDragContinue
                , touchPrevNextMsg = wrapMsg << touchPrevNext dragInfo
                }
                (wrapMsg NoUpdate)
                (wrapMsg << FullMsg)
                { prevImgs = prevImgs
                , album = album
                , viewport = vpInfo.bodyViewport
                , progImgModel = progModel
                , offset = offsetFor dragInfo
                }
                parents
                flags


minDragLen : number
minDragLen =
    -- a bit of experimenting and a bit of HIG googling says ...
    75


touchPrevNext : Maybe ( Touch, Touch ) -> Touch -> AlbumPageMsg
touchPrevNext dragInfo touch =
    case dragInfo of
        Nothing ->
            NoUpdate

        Just ( start, cur ) ->
            if abs (start.clientX - touch.clientX) > minDragLen then
                case getDirectionX start.clientX touch.clientX of
                    Left ->
                        Next

                    Right ->
                        Prev

                    _ ->
                        TouchDragAbandon

            else
                TouchDragAbandon


offsetFor : Maybe ( Touch, Touch ) -> ( Float, Float )
offsetFor dragInfo =
    case dragInfo of
        Nothing ->
            ( 0, 0 )

        Just ( start, current ) ->
            ( current.clientX - start.clientX, current.clientY - start.clientY )


subscriptions : AlbumPage -> (AlbumPageMsg -> msg) -> msg -> Sub msg
subscriptions albumPage wrapper showParent =
    case albumPage of
        Thumbs _ _ _ _ ->
            onEscape showParent <| wrapper NoUpdate

        FullImage _ _ progImgModel _ _ _ ->
            Sub.batch
                [ Sub.map wrapper <| Sub.map FullMsg <| ProgressiveImage.subscriptions progImgModel
                , Sub.map wrapper <|
                    onKeyDown <|
                        Json.Decode.map
                            (\k ->
                                case k of
                                    "ArrowRight" ->
                                        Next

                                    "ArrowLeft" ->
                                        Prev

                                    "Escape" ->
                                        BackToThumbs

                                    _ ->
                                        NoUpdate
                            )
                        <|
                            field "key" string
                ]
