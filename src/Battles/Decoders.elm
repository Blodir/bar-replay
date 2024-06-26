module Battles.Decoders exposing (..)

import Json.Decode exposing (..)
import Array exposing (Array)
import Json.Decode.Pipeline exposing (required)
import Json.Decode.Pipeline exposing (optional)
import Regex

type alias BattlesResponse = Array Battle

type alias Battle =
  { title: String
  , mapFileName: Maybe String
  , map: String
  , gameType: Maybe GameType
  , players: Array Player
  }

type GameType = Team | Duel | FFA

type alias Player =
  { gameStatus: PlayerGameStatus
  , skill: Maybe Float
  , username: String
  }

type PlayerGameStatus = Spectating | Playing | Waiting

battlesDecoder : Decoder BattlesResponse
battlesDecoder = array battleDecoder

battleDecoder : Decoder Battle
battleDecoder =
  succeed Battle
    |> required "title" string
    |> optional "mapFileName" (map (\s -> Just s) string) Nothing
    |> required "map" string
    |> optional "gameType" (map (\s -> Just s) gameTypeDecoder) Nothing
    |> required "players" (array playerDecoder)

playerDecoder : Decoder Player
playerDecoder =
  succeed Player
    |> optional "gameStatus" playerGameStatusDecoder Waiting
    |> optional "skill" (map (\s -> parsePlayerSkill s) string) Nothing
    |> required "username" string

playerGameStatusDecoder : Decoder PlayerGameStatus
playerGameStatusDecoder =
  map (
    \t -> case t of
      "Playing" -> Playing
      "Waiting" -> Waiting
      "Spectating" -> Spectating
      _ -> Waiting
  ) string

gameTypeDecoder : Decoder GameType
gameTypeDecoder =
  map (
    \t -> case t of
      "Duel" -> Duel
      "Team" -> Team
      _ -> FFA
  ) string

parsePlayerSkill : String -> Maybe Float
parsePlayerSkill str =
  let
    regex =
      Maybe.withDefault Regex.never <|
        Regex.fromString "\\[(\\d*\\.\\d*).*\\]"
    extractSubmatch : Regex.Match -> Maybe Float
    extractSubmatch match =
      flattenMaybe (Maybe.map (\s -> String.toFloat s) (flattenMaybe (List.head match.submatches)))
  in
    flattenMaybe
      <| Maybe.map extractSubmatch
      <| List.head
      <| Regex.find regex str

flattenMaybe : Maybe (Maybe a) -> Maybe a
flattenMaybe maybe
  = case maybe of
    Just innerMaybe ->
      case innerMaybe of
        Just value -> Just value
        Nothing -> Nothing
    Nothing -> Nothing
