module LocationUtils exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Url exposing (Protocol(..), Url)
import Utils.LocationUtils exposing (parseOriginRelativeUrl)


exampleDotCom : Url
exampleDotCom =
    { protocol = Https
    , host = "example.com"
    , port_ = Nothing
    , path = "/"
    , query = Nothing
    , fragment = Nothing
    }


suite : Test
suite =
    describe "parseOriginRelativeUrl"
        [ test "slash parses" <|
            \_ ->
                Expect.equal (Just exampleDotCom) <|
                    parseOriginRelativeUrl exampleDotCom "/"
        , test "slash hash parses" <|
            \_ ->
                Expect.equal (Just { exampleDotCom | fragment = Just "" }) <|
                    parseOriginRelativeUrl exampleDotCom "/#"
        , test "slash query hash parses" <|
            \_ ->
                Expect.equal (Just { exampleDotCom | fragment = Just "", query = Just "" }) <|
                    parseOriginRelativeUrl exampleDotCom "/?#"
        , test "non-empty stuff parses" <|
            \_ ->
                Expect.equal (Just { exampleDotCom | path = "/foo", fragment = Just "quux", query = Just "bar=baz" }) <|
                    parseOriginRelativeUrl exampleDotCom "/foo?bar=baz#quux"
        ]
