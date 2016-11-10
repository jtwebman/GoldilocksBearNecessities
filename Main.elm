module Main exposing (..)

import Html exposing (text)
import String


-- MODEL


type alias Model =
    { challenges : List Challenge
    }


type alias Challenge =
    { name : String
    , goldilock : Goldilock
    , chairs : List Chair
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
    parseChallenge "Sample" "100 80\n30 50\n130 75\n90 60\n150 85\n120 70\n200 200\n110 100"


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


parseChairs : List String -> List Chair
parseChairs chairValues =
    List.map parseChair chairValues


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


parseChallenge : String -> String -> Challenge
parseChallenge name values =
    case String.lines values of
        [] ->
            { name = name
            , goldilock = parseGoldilock ""
            , chairs = parseChairs []
            }

        [ goldilock ] ->
            { name = name
            , goldilock = parseGoldilock goldilock
            , chairs = parseChairs []
            }

        goldilock :: chairs ->
            { name = name
            , goldilock = parseGoldilock goldilock
            , chairs = parseChairs chairs
            }


main =
    text sampleChallenge.name
