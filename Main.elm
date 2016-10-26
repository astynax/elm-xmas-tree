module Main exposing (..)

import Debug
import List
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Random exposing (Generator, Seed, initialSeed, generate)
import String
import Time exposing (Time)
import Html exposing (Html, span, div, text, a)
import Html.App exposing (program)
import Html.Attributes exposing (id, class, href)


type Elem
    = Fur Bool Int
    | Toy1
    | Toy2
    | Toy3
    | Star


type alias Pine =
    List ( Int, List Elem )


type alias Model =
    Pine


type Msg
    = Tick
    | NewPine Pine


main : Program Never
main =
    program
        { init = update Tick []
        , update = update
        , subscriptions = always ticks
        , view = view
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( model, generatePine 25 )

        NewPine pine ->
            ( pine, Cmd.none )


ticks : Sub Msg
ticks =
    Time.every (5 * Time.second) <| always Tick


view : Model -> Html Msg
view pine =
    div []
        [ div [ id "happynewyear" ] [ text "Happy New Year!" ]
        , div [ id "pine-container" ] [ render pine ]
        , div
            [ id "footer"
            ]
            [ a
                [ href "https://github.com/astynax/elm-xmas-tree"
                ]
                [ text "Want to grow a copy? Go and clone!" ]
            ]
        ]


generatePine : Int -> Cmd Msg
generatePine height =
    generate NewPine (pine height)


pine : Int -> Generator Pine
pine n =
    let
        go cnt =
            if cnt > n then
                forever []
            else
                Random.andThen
                    (row cnt)
                    (\r ->
                        Random.map
                            ((::) ( n - cnt, r ))
                            (go (cnt + 1))
                    )
    in
        go 1


row : Int -> Generator (List Elem)
row n =
    case n of
        1 ->
            forever [ Star ]

        2 ->
            Random.map (\x -> [ Fur False 1, x, Fur True 1 ]) toy

        3 ->
            Random.map (\x -> [ Fur False 2, x, Fur True 2 ]) toy

        _ ->
            Random.map fixDirection <| chain (n * 2 - 1) False


render : Pine -> Html Msg
render =
    div [ id "pine" ]
        << List.map
            (\( s, r ) ->
                div []
                    ((span [] [ text <| String.repeat s " " ])
                        :: List.map renderElem r
                    )
            )


renderElem : Elem -> Html Msg
renderElem el =
    span
        [ class <|
            String.append "decoration " <|
                case el of
                    Star ->
                        "star"

                    Toy1 ->
                        "toy1"

                    Toy2 ->
                        "toy2"

                    Toy3 ->
                        "toy3"

                    _ ->
                        "fur"
        ]
        [ text <|
            case el of
                Star ->
                    "*"

                Toy1 ->
                    "o"

                Toy2 ->
                    "O"

                Toy3 ->
                    "@"

                Fur False 1 ->
                    ">"

                Fur False 2 ->
                    ">>"

                Fur False 3 ->
                    ">>>"

                Fur True 1 ->
                    "<"

                Fur True 2 ->
                    "<<"

                Fur True 3 ->
                    "<<<"

                _ ->
                    toString el
        ]


oneOf : List a -> Generator a
oneOf xs =
    flip Random.map (Random.int 0 (List.length xs - 1)) <|
        \n ->
            case List.drop n xs of
                x :: _ ->
                    x

                _ ->
                    Debug.crash "Oops!"


forever : a -> Generator a
forever =
    flip Random.map Random.bool << always


fur : Generator Elem
fur =
    Random.map2 Fur Random.bool (Random.int 1 3)


toy : Generator Elem
toy =
    oneOf [ Toy1, Toy2, Toy3, Star ]


chain : Int -> Bool -> Generator (List Elem)
chain n isToy =
    if n > 5 then
        Random.andThen
            (if isToy then
                toy
             else
                fur
            )
            (\el ->
                let
                    n' =
                        n - elemLen el
                in
                    Random.map ((::) el) (chain n' (not isToy))
            )
    else if isToy then
        if n <= 4 then
            Random.map (\t -> [ t, Fur True (n - 1) ]) toy
        else
            Random.map3
                (\t1 f t2 ->
                    [ t1
                    , f
                    , t2
                    , Fur True (3 - elemLen f)
                    ]
                )
                toy
                (Random.map2
                    Fur
                    Random.bool
                    (Random.int 1 2)
                )
                toy
    else
        Random.andThen
            fur
            (\f ->
                Random.map ((::) f) <|
                    chain (n - elemLen f) True
            )


elemLen : Elem -> Int
elemLen el =
    case el of
        Fur _ n ->
            n

        _ ->
            1


fixDirection : List Elem -> List Elem
fixDirection es =
    case es of
        (Fur _ n) :: es' ->
            Fur False n :: es'

        _ ->
            es
