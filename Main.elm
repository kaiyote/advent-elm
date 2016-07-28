module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Dict as D exposing (Dict)
import String as S
import List as L
import Array as A
import Set
import Lazy.List as LL


type alias Flags =
  { inputs : List (String, String)
  }


type alias Model =
  { solutions : Dict String (Int, Int)
  , inputs : Dict String String
  }


type Msg
  = Compute String (String -> Int) (String -> Int)


main : Program Flags
main =
  Html.programWithFlags
    { init = init
    , update = update
    , subscriptions = (\_ -> Sub.none)
    , view = view
    }


init : Flags -> (Model, Cmd Msg)
init flags =
  Model D.empty (D.fromList flags.inputs) ! []


view : Model -> Html Msg
view model =
  List.map (\f -> f model)
    [ dayChunk "day 1" day1Part1 day1Part2
    , dayChunk "day 2" day2Part1 day2Part2
    , dayChunk "day 3" day3Part1 day3Part2
    , dayChunk "day 4" day4Part1 day4Part2
    ] |> div []


dayChunk : String -> (String -> Int) -> (String -> Int) -> Model -> Html Msg
dayChunk disp part1 part2 model =
  let
    key =
      S.filter (\c -> c /= ' ') disp
  in
    div []
      [ button [ onClick <| Compute key part1 part2 ] [ text disp ]
      , answerChunk key model.solutions
      ]


answerChunk : String -> Dict String (Int, Int) -> Html Msg
answerChunk key model =
  case D.get key model of
    Nothing -> text "   Nothing to see here"
    Just (0, 0) -> text "   Something to see here, but the computations have not been done"
    Just something -> text <| "   Part 1: " ++ toString (fst something) ++ ", Part 2: " ++ toString (snd something)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Compute key part1 part2 ->
      { model | solutions = D.insert key (compute part1 part2 key model.inputs) model.solutions } ! []


compute : (String -> Int) -> (String -> Int) -> String -> Dict String String -> (Int, Int)
compute part1Func part2Func key dict =
  case D.get key dict of
    Nothing -> (0, 0)
    Just input ->
      (part1Func input, part2Func input)


day1Part1 : String -> Int
day1Part1 input =
  S.toList input
    |> L.foldl (\c total -> if c == '(' then total + 1 else total - 1) 0


day1Part2 : String -> Int
day1Part2 input =
  S.toList input
    |> A.fromList
    |> A.toIndexedList
    |> L.foldl (\(ind, c) (total, index) ->
        if total /= -1 then
          (if c == '(' then total + 1 else total - 1, ind + 1)
        else
          (total, index)
      ) (0, 0)
    |> snd


day2Part1 : String -> Int
day2Part1 input =
  S.lines input
    |> L.map (S.split "x")
    |> L.map (L.map S.toInt)
    |> L.map (L.map (Result.withDefault 0))
    |> L.map L.sort
    |> L.map areaFromList
    |> L.foldl (+) 0


areaFromList : List Int -> Int
areaFromList ints =
  case ints of
    l :: w :: h :: [] ->
      2 * l * w + 2 * w * h + 2 * l * h + l * w
    _ ->
      0


day2Part2 : String -> Int
day2Part2 input =
  S.lines input
    |> L.map (S.split "x")
    |> L.map (L.map S.toInt)
    |> L.map (L.map (Result.withDefault 0))
    |> L.map L.sort
    |> L.map bowLengthFromList
    |> L.foldl (+) 0


bowLengthFromList : List Int -> Int
bowLengthFromList ints =
  case ints of
    l :: w :: h :: [] ->
      2 * l + 2 * w + l * w * h
    _ ->
      0


day3Part1 : String -> Int
day3Part1 input =
  S.toList input
    |> L.scanl (\c (x, y) ->
        case c of
          '^' -> (x, y - 1)
          'v' -> (x, y + 1)
          '<' -> (x - 1, y)
          '>' -> (x + 1, y)
          _ -> (x, y)
      ) (0, 0)
    |> Set.fromList
    |> Set.size


day3Part2 : String -> Int
day3Part2 input =
  S.toList input
    |> A.fromList
    |> A.toIndexedList
    |> L.partition (\(i, _) -> i % 2 == 0)
    |> (\(a, b) -> a :: b :: [])
    |> L.map (L.map (\(_, c) -> c))
    |> L.concatMap (L.scanl (\c (x, y) ->
          case c of
            '^' -> (x, y - 1)
            'v' -> (x, y + 1)
            '<' -> (x - 1, y)
            '>' -> (x + 1, y)
            _ -> (x, y)
        ) (0, 0)
      )
    |> Set.fromList
    |> Set.size


day4Part1 : String -> Int
day4Part1 input =
  0


day4Part2 : String -> Int
day4Part2 input =
  0
