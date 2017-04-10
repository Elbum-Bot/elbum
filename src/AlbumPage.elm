module AlbumPage exposing (AlbumPage(..), AlbumPageMsg(..), view, update, subscriptions)

import ListUtils exposing (..)
import WinSize exposing (..)
import Album exposing (..)
import ThumbPage exposing (..)
import FullImagePage exposing (..)
import Html exposing (..)
import Keyboard exposing (..)
import TouchEvents exposing (..)


type AlbumPage
    = Thumbs Album WinSize
    | FullImage (List Image) Album WinSize (Maybe ( Touch, Touch ))


type AlbumPageMsg
    = View (List Image) Image (List Image)
    | TouchDragStart Touch
    | TouchDragContinue Touch
    | Prev
    | Next
    | BackToThumbs
    | NoUpdate



--note: no commands here


update : AlbumPageMsg -> AlbumPage -> AlbumPage
update msg model =
    case msg of
        View prevImgs curImg nextImgs ->
            case model of
                Thumbs album winSize ->
                    FullImage
                        prevImgs
                        { title = album.title
                        , imageFirst = curImg
                        , imageRest = nextImgs
                        }
                        winSize
                        Nothing

                _ ->
                    model

        Prev ->
            case model of
                FullImage prevImgs album winSize _ ->
                    let
                        ( newPrev, newCur, newRest ) =
                            shiftLeft prevImgs album.imageFirst album.imageRest
                    in
                        FullImage
                            newPrev
                            { title = album.title
                            , imageFirst = newCur
                            , imageRest = newRest
                            }
                            winSize
                            Nothing

                _ ->
                    model

        Next ->
            case model of
                FullImage prevImgs album winSize _ ->
                    let
                        ( newPrev, newCur, newRest ) =
                            shiftRight prevImgs album.imageFirst album.imageRest
                    in
                        FullImage
                            newPrev
                            { title = album.title
                            , imageFirst = newCur
                            , imageRest = newRest
                            }
                            winSize
                            Nothing

                _ ->
                    model

        BackToThumbs ->
            case model of
                FullImage prevImgs album winSize _ ->
                    let
                        ( newFirst, newRest ) =
                            shiftToBeginning prevImgs album.imageFirst album.imageRest
                    in
                        Thumbs
                            { title = album.title
                            , imageFirst = newFirst
                            , imageRest = newRest
                            }
                            winSize

                _ ->
                    model

        TouchDragStart pos ->
            case model of
                FullImage prevImgs album winSize dragInfo ->
                    FullImage prevImgs album winSize (Just ( pos, pos ))

                _ ->
                    model

        TouchDragContinue pos ->
            case model of
                FullImage prevImgs album winSize dragInfo ->
                    case dragInfo of
                        Nothing ->
                            FullImage prevImgs album winSize (Just ( pos, pos ))

                        Just ( start, cur ) ->
                            FullImage prevImgs album winSize (Just ( start, pos ))

                _ ->
                    model

        NoUpdate ->
            model


view : AlbumPage -> List AlbumTreeNode -> Html AlbumPageMsg
view albumPage parents =
    case albumPage of
        Thumbs album winSize ->
            ThumbPage.view View { album = album, parents = parents, winSize = winSize }

        FullImage prevImgs album winSize dragInfo ->
            FullImagePage.view
                Prev
                Next
                BackToThumbs
                TouchDragStart
                TouchDragContinue
                (touchPrevNext dragInfo)
                NoUpdate
                { prevImgs = prevImgs
                , album = album
                , winSize = winSize
                , offset = offsetFor dragInfo
                }


touchPrevNext : Maybe ( Touch, Touch ) -> Touch -> AlbumPageMsg
touchPrevNext dragInfo touch =
    case dragInfo of
        Nothing ->
            NoUpdate

        Just ( start, cur ) ->
            case getDirectionX start.clientX touch.clientX of
                Left ->
                    Next

                Right ->
                    Prev

                _ ->
                    NoUpdate


offsetFor : Maybe ( Touch, Touch ) -> ( Float, Float )
offsetFor dragInfo =
    case dragInfo of
        Nothing ->
            ( 0, 0 )

        Just ( start, current ) ->
            ( current.clientX - start.clientX, current.clientY - start.clientY )


subscriptions : AlbumPage -> Sub AlbumPageMsg
subscriptions albumPage =
    case albumPage of
        Thumbs _ _ ->
            Sub.none

        FullImage _ _ _ _ ->
            downs
                (\keycode ->
                    case keycode of
                        39 ->
                            -- right arrow
                            Next

                        37 ->
                            -- left arrow
                            Prev

                        38 ->
                            -- up arrow
                            BackToThumbs

                        _ ->
                            NoUpdate
                )
