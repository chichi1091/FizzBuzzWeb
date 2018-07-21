module Main exposing (..)

import Html exposing (Html, text, div, input, table, thead, tbody, tr, th, td)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput, onClick)

---- MODEL ----
initial = 100
type alias Model =
    {
        maxVal : Int
        , widthVal : Int
        , heightVal : Int
    }

init : ( Model, Cmd Msg )
init =
    ({
    maxVal = initial
    , widthVal = 10
    , heightVal = (initial // 10) - 1
    }
    , Cmd.none )



---- UPDATE ----
type Msg
    = InputMax String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputMax val ->
            let
                newMax = (Result.withDefault 0 (String.toInt val))
                newHeight = (newMax // 10) - 1
            in
                ({ model | maxVal = newMax, heightVal = newHeight}, Cmd.none)



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        th2List x =
            th[] [text (toString x)]
        
        td2tagList =
            let
                judgment x =
                    if (x % 3) == 0 && (x % 5) == 0 then
                        "FizzBuzz"
                    else if (x % 3) == 0 then
                        "Fizz"
                    else if (x % 5) == 0 then
                        "Buzz"
                    else
                        toString x

                tagList y =
                    List.map (\x -> td[] [text (judgment (y + x))])
                    <| List.range 1 10
                
            in
                List.map (\x -> tr[] <| tagList (x * 10)) <| List.range 0 model.heightVal
        
        
        table2tag =
            table[]
                [
                   thead[][
                       tr[] <| List.map th2List <| List.range 1 model.widthVal
                   ] 
                   , tbody[]
                       td2tagList
                ]

    in
        div []
            [  
                text "1 ー "
                , input [value (toString model.maxVal) ,onInput InputMax][]
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
