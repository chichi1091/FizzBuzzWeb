module Main exposing (..)

import Html exposing (Html, text, div, input, table, thead, tbody, tr, th, td, select, option)
import Html.Attributes exposing (value, class, selected)
import Html.Events exposing (onInput, onClick)

---- MODEL ----
initial : Int
initial = 100
type alias Model =
    {
        maxVal : Int
        , widthVal : Int
        , heightVal : Int
        , fizzVal : Int
        , buzzVal : Int
    }

init : ( Model, Cmd Msg )
init =
    ({
    maxVal = initial
    , widthVal = 10
    , heightVal = (initial // 10) - 1
    , fizzVal = 3
    , buzzVal = 5
    }
    , Cmd.none )



---- UPDATE ----
type Msg
    = InputMax String
    | InputFizz String
    | InputBuzz String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputMax val ->
            let
                newMax = (Result.withDefault 0 (String.toInt val))
                newHeight = (newMax // 10) - 1
            in
                ({ model | maxVal = newMax, heightVal = newHeight}, Cmd.none)
        InputFizz val ->
            let
                newVal = (Result.withDefault 1 (String.toInt val))
            in
                ({ model | fizzVal = newVal}, Cmd.none)
        InputBuzz val ->
            let
                newVal = (Result.withDefault 1 (String.toInt val))
            in
                ({ model | buzzVal = newVal}, Cmd.none)



---- VIEW ----
view : Model -> Html Msg
view model =
    let
        th2List x =
            th[] [text (toString x)]
        
        td2tagList =
            let
                judgment x =
                    if (x % model.fizzVal) == 0 && (x % model.buzzVal) == 0 then
                        "FizzBuzz"
                    else if (x % model.fizzVal) == 0 then
                        "Fizz"
                    else if (x % model.buzzVal) == 0 then
                        "Buzz"
                    else
                        toString x

                tagList y =
                    List.map (\x -> td[class <| (judgment (y + x))] [text (judgment (y + x))])
                    <| List.range 1 10
                
            in
                List.map (\x -> tr[] <| tagList (x * 10)) <| List.range 0 model.heightVal
        
        
        table2tag =
            table[]
                [
                --    thead[][
                --        tr[] <| List.map th2List <| List.range 1 model.widthVal
                --    ] 
                   tbody[]
                       td2tagList
                ]

    in
        div []
            [  
                text "1 ー "
                , input [value (toString model.maxVal) ,onInput InputMax][]
                , text "Fizz:"
                , select [onInput InputFizz]
                    <| List.map (\x ->
                        let
                            select x =
                                if x == model.fizzVal then
                                    True
                                else
                                    False
                        in
                            option[value (toString x), selected (select x)][text (toString x)])
                        <| List.range 1 9
                , text "Buzz:"
                , select [onInput InputBuzz]
                    <| List.map (\x ->
                        let
                            select x =
                                if x == model.buzzVal then
                                    True
                                else
                                    False
                        in
                            option[value (toString x), selected (select x)][text (toString x)])
                        <| List.range 1 9
                , table2tag
            ]

---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
