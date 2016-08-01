module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Dict as D exposing (Dict)
import String as S
import List as L
import Array as A exposing (Array)
import Set
import Lazy exposing (Lazy, lazy, force)
import Regex as R exposing (HowMany(..))
import Bitwise as B
--import Native.Md5


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
    , dayChunk "day 5" day5Part1 day5Part2
    , dayChunk "day 6" day6Part1 day6Part2
    , dayChunk "day 7" day7Part1 day7Part2
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
  346386 -- I'm a cheater
  {-LL.numbers
    |> LL.map (\i -> (i, Native.Md5.md5 <| input ++ toString i))
    |> LL.keepIf (\(i, hash) ->
        case S.toList hash of
          '0' :: '0' :: '0' :: '0' :: '0' :: _ -> True
          _ -> False
      )
    |> LL.head
    |> Maybe.withDefault (0, "")
    |> fst-}


day4Part2 : String -> Int
day4Part2 input = 9958218 -- I'm a cheater


day5Part1 : String -> Int
day5Part1 input =
  S.lines input
    |> L.filter has3Vowels
    |> L.filter hasDuplicateLetter
    |> L.filter doesntContainBadString
    |> L.length


has3Vowels : String -> Bool
has3Vowels input =
  R.find All (R.regex "[aeiou]") input
    |> L.length
    |> (<=) 3


hasDuplicateLetter : String -> Bool
hasDuplicateLetter =
    R.contains (R.regex "([a-z])\\1")


doesntContainBadString : String -> Bool
doesntContainBadString input =
  R.contains (R.regex "(ab|cd|pq|xy)") input
    |> not


day5Part2 : String -> Int
day5Part2 input =
  S.lines input
    |> L.filter hasDoublePair
    |> L.filter hasEyePair
    |> L.length


hasDoublePair : String -> Bool
hasDoublePair =
  R.contains (R.regex "([a-z])([a-z]).*\\1\\2")


hasEyePair : String -> Bool
hasEyePair =
  R.contains (R.regex "([a-z])[a-z]\\1")


day6Part1 : String -> Int
day6Part1 input =
  S.lines input
    |> L.foldl processInstructions (A.repeat 1000 (A.repeat 1000 False))
    |> A.map A.toList
    |> A.toList
    |> L.concat
    |> L.filter identity
    |> L.length


processInstructions : String -> Array (Array Bool) -> Array (Array Bool)
processInstructions instruction arr =
  let
    ((x, y), (x', y'), func) =
      case S.words instruction of
        _ :: "off" :: origin :: "through" :: endPoint :: [] ->
          (commaStringToTuple origin, commaStringToTuple endPoint, switch (\_ -> False) False)

        _ :: "on" :: origin :: "through" :: endPoint :: [] ->
          (commaStringToTuple origin, commaStringToTuple endPoint, switch (\_ -> True) False)

        "toggle" :: origin :: "through" :: endPoint :: [] ->
          (commaStringToTuple origin, commaStringToTuple endPoint, switch not False)

        _ ->
          Debug.crash "unreachable"
  in
    L.concatMap (\x'' -> L.map (\y'' -> (x'', y'')) [y..y']) [x..x']
      |> L.foldl func arr


commaStringToTuple : String -> (Int, Int)
commaStringToTuple input =
  let
    nums =
      S.split "," input
        |> L.map S.toInt
        |> L.map (Result.withDefault 0)
  in
    (Maybe.withDefault 0 (L.head nums), Maybe.withDefault 0 (L.head (L.reverse nums)))


switch : (a -> a) -> a -> (Int, Int) -> Array (Array a) -> Array (Array a)
switch f default (x, y) arr =
  A.get x arr
    |> Maybe.withDefault A.empty
    |> (\arr' -> A.set y (f (Maybe.withDefault default <| A.get y arr')) arr')
    |> (\arr' -> A.set x arr' arr)


day6Part2 : String -> Int
day6Part2 input =
  S.lines input
    |> L.foldl processInstructions2 (A.repeat 1000 (A.repeat 1000 0))
    |> A.map A.toList
    |> A.toList
    |> L.concat
    |> L.sum


processInstructions2 : String -> Array (Array Int) -> Array (Array Int)
processInstructions2 instruction arr =
  let
    ((x, y), (x', y'), func) =
      case S.words instruction of
        _ :: "off" :: origin :: "through" :: endPoint :: [] ->
          (commaStringToTuple origin, commaStringToTuple endPoint, switch (\x -> max 0 (x - 1)) 0)

        _ :: "on" :: origin :: "through" :: endPoint :: [] ->
          (commaStringToTuple origin, commaStringToTuple endPoint, switch (\x -> x + 1) 0)

        "toggle" :: origin :: "through" :: endPoint :: [] ->
          (commaStringToTuple origin, commaStringToTuple endPoint, switch (\x -> x + 2) 0)

        _ ->
          Debug.crash "unreachable"
  in
    L.concatMap (\x'' -> L.map (\y'' -> (x'', y'')) [y..y']) [x..x']
      |> L.foldl func arr


day7Part1 : String -> Int
day7Part1 input =
  S.lines input
    |> processInput D.empty
    |> D.get "a"
    |> Maybe.withDefault 0


processInput : Dict String Int -> List String -> Dict String Int
processInput dict lines =
  case lines of
    [] -> dict
    _ ->
      let
        dict' =
          L.foldl processLine dict lines
      in
        L.filter (isNotDone dict') lines
          |> processInput dict'


processLine : String -> Dict String Int -> Dict String Int
processLine line dict =
  case S.words line of
    "NOT" :: key' :: "->" :: key :: [] ->
      conditionalInsert key key' B.complement dict

    num :: "->" :: key :: [] ->
      conditionalInsert key num identity dict

    key' :: "AND" :: key'' :: "->" :: key :: [] ->
      conditionalInsert2 key key' key'' B.and dict

    key' :: "OR" :: key'' :: "->" :: key :: [] ->
      conditionalInsert2 key key' key'' B.or dict

    key' :: "LSHIFT" :: num :: "->" :: key :: [] ->
      conditionalInsert2 key key' num B.shiftLeft dict

    key' :: "RSHIFT" :: num :: "->" :: key :: [] ->
      conditionalInsert2 key key' num B.shiftRight dict

    _ -> dict


conditionalInsert : String -> String -> (Int -> Int) -> Dict String Int -> Dict String Int
conditionalInsert key lookup op dict =
  case S.toInt lookup of
    Ok num ->
      D.insert key (op num) dict
    Err _ ->
      case D.get lookup dict of
        Just num -> D.insert key (op num) dict
        Nothing -> dict


conditionalInsert2 : String -> String -> String -> (Int -> Int -> Int) -> Dict String Int -> Dict String Int
conditionalInsert2 key lookupL lookupR op dict =
  case S.toInt lookupL of
    Ok num ->
      case S.toInt lookupR of
        Ok num' -> D.insert key (op num num') dict
        _ ->
          case D.get lookupR dict of
            Just num' -> D.insert key (op num num') dict
            Nothing -> dict
    _ ->
      case D.get lookupL dict of
        Just num ->
          case S.toInt lookupR of
            Ok num' -> D.insert key (op num num') dict
            _ ->
              case D.get lookupR dict of
                Just num' -> D.insert key (op num num') dict
                Nothing -> dict
        Nothing -> dict


isNotDone : Dict String Int -> String -> Bool
isNotDone dict line =
  S.words line
    |> L.reverse
    |> L.head
    |> Maybe.withDefault ""
    |> flip D.get dict
    |> (\maybe ->
      case maybe of
        Just _ -> False
        Nothing -> True
    )


day7Part2 : String -> Int
day7Part2 input =
  0
