module Main exposing (..)

import Browser
import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (class, href, id)
import List
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Random exposing (Generator, Seed, generate, initialSeed)
import String
import Time exposing (Posix)


type FurSize
    = S1
    | S2
    | S3


type Elem
    = Fur Bool FurSize
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


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> update Tick []
        , update = update
        , subscriptions = always ticks
        , view = view
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( model, generatePine 25 )

        NewPine p ->
            ( p, Cmd.none )


ticks : Sub Msg
ticks =
    Time.every 5000 <| always Tick


view : Model -> Html Msg
view model =
    div []
        [ div [ id "happynewyear" ] [ text "Happy New Year!" ]
        , div [ id "pine-container" ] [ render model ]
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
                    (\r ->
                        Random.map
                            ((::) ( n - cnt, r ))
                            (go (cnt + 1))
                    )
                    (row cnt)
    in
    go 1


row : Int -> Generator (List Elem)
row n =
    case n of
        1 ->
            forever [ Star ]

        2 ->
            Random.map (\x -> [ Fur False S1, x, Fur True S1 ]) toy

        3 ->
            Random.map (\x -> [ Fur False S2, x, Fur True S2 ]) toy

        _ ->
            Random.map fixDirection <| chain (n * 2 - 1) False


render : Pine -> Html Msg
render =
    div [ id "pine" ]
        << List.map
            (\( s, r ) ->
                div []
                    (span [] [ text <| String.repeat s " " ]
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

                Fur False S1 ->
                    ">"

                Fur False S2 ->
                    ">>"

                Fur False S3 ->
                    ">>>"

                Fur True S1 ->
                    "<"

                Fur True S2 ->
                    "<<"

                Fur True S3 ->
                    "<<<"
        ]


bool : Generator Bool
bool =
    Random.uniform True [ False ]


forever : a -> Generator a
forever x =
    Random.uniform x []


fur : Generator Elem
fur =
    Random.map2 Fur bool (Random.uniform S1 [ S2, S3 ])


toy : Generator Elem
toy =
    Random.uniform Toy1 [ Toy2, Toy3, Star ]


chain : Int -> Bool -> Generator (List Elem)
chain n isToy =
    if n > 5 then
        (if isToy then
            toy

         else
            fur
        )
            |> Random.andThen
                (\el ->
                    let
                        nn =
                            n - elemLen el
                    in
                    Random.map ((::) el) (chain nn (not isToy))
                )

    else if isToy then
        if n <= 4 then
            Random.map (\t -> [ t, Fur True <| fromInt (n - 1) ]) toy

        else
            Random.map3
                (\t1 f t2 ->
                    [ t1
                    , f
                    , t2
                    , Fur True <| fromInt <| 3 - elemLen f
                    ]
                )
                toy
                (Random.map2
                    Fur
                    bool
                    (Random.uniform S1 [ S2 ])
                )
                toy

    else
        Random.andThen
            (\f ->
                Random.map ((::) f) <|
                    chain (n - elemLen f) True
            )
            fur


fromInt : Int -> FurSize
fromInt x =
    case x of
        1 ->
            S1

        2 ->
            S2

        _ ->
            S3


elemLen : Elem -> Int
elemLen el =
    case el of
        Fur _ S1 ->
            1

        Fur _ S2 ->
            2

        Fur _ S3 ->
            3

        _ ->
            1


fixDirection : List Elem -> List Elem
fixDirection es =
    case es of
        (Fur _ n) :: ns ->
            Fur False n :: ns

        _ ->
            es
