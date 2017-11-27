import Elm.Module
import Elm.Versions

import Data.Proxy

import AlbumTypes

main :: IO ()
main =
    putStrLn $ makeElmModuleWithVersion Elm0p18 "Album"
    [ DefineElm (Proxy :: Proxy AlbumTreeNode)
    , DefineElm (Proxy :: Proxy NodeOrAlbum)
    , DefineElm (Proxy :: Proxy Album)
    , DefineElm (Proxy :: Proxy Image)
    , DefineElm (Proxy :: Proxy ImgSrc)
    ]