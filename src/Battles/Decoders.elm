module Battles.Decoders exposing (..)

import Json.Decode exposing (..)
import Array exposing (Array)
import Json.Decode.Pipeline exposing (required)
import Json.Decode.Pipeline exposing (optional)

type alias BattlesResponse = Array Battle

type alias Battle =
  { title: String
  , mapFileName: Maybe String
  , gameType: Maybe GameType
  , players: Array Player
  }

type GameType = Team | Duel | FFA

type alias Player =
  { gameStatus: PlayerGameStatus
  , skill: Maybe String
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
    |> optional "gameType" (map (\s -> Just s) gameTypeDecoder) Nothing
    |> required "players" (array playerDecoder)

playerDecoder : Decoder Player
playerDecoder =
  succeed Player
    |> optional "gameStatus" playerGameStatusDecoder Waiting
    |> optional "skill" (map (\s -> Just s) string) Nothing
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
