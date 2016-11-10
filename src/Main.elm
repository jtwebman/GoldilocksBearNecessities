module Main exposing (..)

import Html exposing (Html, button, div, text)
import String
import Array
import Html.App as App


main =
    App.beginnerProgram { model = startingModel, view = view, update = update }



-- MODEL


type alias Model =
    { challenges : List Challenge
    }


type alias Challenge =
    { name : String
    , goldilock : Goldilock
    , chairs : List ( Int, Chair )
    , answer : List Int
    }


type alias Goldilock =
    { weight : Int
    , maxTemperature : Int
    }


type alias Chair =
    { weightCapacity : Int
    , porridgeTemperature : Int
    }


startingModel : Model
startingModel =
    { challenges =
        [ sampleChallenge ]
    }


sampleChallenge =
    "100 80\n30 50\n130 75\n90 60\n150 85\n120 70\n200 200\n110 100"
        |> getChallenge "Sample"
        |> getAnswer


parseGoldilock : String -> Goldilock
parseGoldilock value =
    case String.words value of
        [] ->
            { weight = 0, maxTemperature = 0 }

        [ weight ] ->
            { weight = Result.withDefault 0 (String.toInt weight)
            , maxTemperature = 0
            }

        [ weight, temperature ] ->
            { weight = Result.withDefault 0 (String.toInt weight)
            , maxTemperature = Result.withDefault 0 (String.toInt temperature)
            }

        weight :: temperature :: _ ->
            { weight = Result.withDefault 0 (String.toInt weight)
            , maxTemperature = Result.withDefault 0 (String.toInt temperature)
            }


parseChairs : List String -> List ( Int, Chair )
parseChairs chairs =
    chairs
        |> List.map parseChair
        |> Array.fromList
        |> Array.toIndexedList
        |> List.map (\( index, chair ) -> ( index + 1, chair ))


parseChair : String -> Chair
parseChair chair =
    case String.words chair of
        [] ->
            { weightCapacity = 0, porridgeTemperature = 0 }

        [ weight ] ->
            { weightCapacity = Result.withDefault 0 (String.toInt weight)
            , porridgeTemperature = 0
            }

        [ weight, temperature ] ->
            { weightCapacity = Result.withDefault 0 (String.toInt weight)
            , porridgeTemperature = Result.withDefault 0 (String.toInt temperature)
            }

        weight :: temperature :: _ ->
            { weightCapacity = Result.withDefault 0 (String.toInt weight)
            , porridgeTemperature = Result.withDefault 0 (String.toInt temperature)
            }


getChallenge : String -> String -> Challenge
getChallenge name values =
    case String.lines values of
        [] ->
            { name = name
            , goldilock = parseGoldilock ""
            , chairs = parseChairs []
            , answer = []
            }

        [ goldilock ] ->
            { name = name
            , goldilock = parseGoldilock goldilock
            , chairs = parseChairs []
            , answer = []
            }

        goldilock :: chairs ->
            { name = name
            , goldilock = parseGoldilock goldilock
            , chairs = parseChairs chairs
            , answer = []
            }


getAnswer : Challenge -> Challenge
getAnswer challenge =
    { challenge | answer = getAnseerChairs challenge.goldilock challenge.chairs }


getAnseerChairs : Goldilock -> List ( Int, Chair ) -> List Int
getAnseerChairs goldilock chairs =
    chairs
        |> List.filter (chairValid goldilock)
        |> List.map (\( index, chair ) -> index)


chairValid : Goldilock -> ( Int, Chair ) -> Bool
chairValid goldilock ( index, chair ) =
    goldilock.weight
        <= chair.weightCapacity
        && goldilock.maxTemperature
        >= chair.porridgeTemperature



-- UPDATE


type Msg
    = FindChairs


update : Msg -> Model -> Model
update msg model =
    case msg of
        FindChairs ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text (toString model) ] ]
