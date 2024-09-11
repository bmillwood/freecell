module Model exposing (..)

import Array exposing (Array)
import Random
import Task

import Drag

type Suit
  = Spades
  | Hearts
  | Diamonds
  | Clubs

isRed : Suit -> Bool
isRed suit = suit == Hearts || suit == Diamonds

type alias Card = { suit : Suit, rank : Int }

allCards : Array Card
allCards =
  [Spades, Hearts, Diamonds, Clubs]
  |> List.concatMap (\suit ->
      List.range 1 13
      |> List.map (\i -> { suit = suit, rank = i })
    )
  |> Array.fromList

genDeck : Random.Generator (List Card)
genDeck =
  let
    go remaining soFar =
      case Array.get 0 remaining of
        Nothing -> Random.constant soFar
        Just first ->
          Random.int 0 (Array.length remaining - 1)
          |> Random.andThen (\i ->
              let
                before = Array.slice 0 i remaining
                at = Array.get i remaining |> Maybe.withDefault first
                after = Array.slice (i + 1) (Array.length remaining) remaining
              in
              go (Array.append before after) (at :: soFar)
            )
  in
  go allCards []

type alias Game =
  { foundations : Array Card
  , freeCells : Array (Maybe Card)
  , cascades : Array (List Card)
  }

emptyGame : Game
emptyGame =
  { foundations =
      [Spades, Hearts, Diamonds, Clubs]
        |> List.map (\suit -> { suit = suit, rank = 0 })
        |> Array.fromList
  , freeCells = Array.repeat 4 Nothing
  , cascades = Array.empty
  }

cascadesOfDeck : Int -> List Card -> Array (List Card)
cascadesOfDeck numCascades cards =
  let
    add card cascades =
      case Array.get (numCascades - 1) cascades of
        Nothing -> cascades
        Just last ->
          Array.append
            (Array.repeat 1 (card :: last))
            (Array.slice 0 -1 cascades)
  in
  List.foldl add (Array.repeat numCascades []) cards

sequenceCompatible : Card -> Card -> Bool
sequenceCompatible src dst =
  isRed src.suit /= isRed dst.suit && src.rank + 1 == dst.rank

initialSequenceLength : List Card -> Int
initialSequenceLength cards =
  case cards of
    [] -> 0
    [_] -> 1
    c1 :: c2 :: rest ->
      if sequenceCompatible c1 c2
      then 1 + initialSequenceLength (c2 :: rest)
      else 1

type FromLocation
  = FromFoundation Int
  | FromFreeCell Int
  | FromCascade Int Int

type DropLocation
  = ToFoundation
  | ToFreeCell Int
  | ToCascade Int

dropLocation : FromLocation -> DropLocation
dropLocation src =
  case src of
    FromFoundation _ -> ToFoundation
    FromFreeCell i -> ToFreeCell i
    FromCascade i _ -> ToCascade i

type alias Model =
  { errors : List String
  , history : List (Game, List Game)
  , drag : Drag.Model FromLocation DropLocation
  , highlightSeq : Bool
  , highlightFoundation : Bool
  , autoMoveFoundation : Bool
  }

gameOfDeck : List Card -> Game
gameOfDeck cards = { emptyGame | cascades = cascadesOfDeck 8 cards }

type alias DragMsg = Drag.Msg FromLocation DropLocation

cardsFromSource : Game -> FromLocation -> List Card
cardsFromSource game loc =
  case loc of
    FromFoundation i ->
      case Array.get i game.foundations of
        Nothing -> []
        Just card ->
          if card.rank == 0
          then []
          else [card]
    FromFreeCell i ->
      case Array.get i game.freeCells |> Maybe.andThen identity of
        Nothing -> []
        Just card -> [card]
    FromCascade i count ->
      case Array.get i game.cascades of
        Nothing -> []
        Just cards -> List.take count cards

type OneMsg
  = AddError String
  | RequestNewGame
  | NewGame Game
  | AppendGame Game
  | Drag DragMsg
  | TryMove FromLocation DropLocation
  | SetHighlightSeq Bool
  | SetHighlightFoundation Bool
  | SetAutoMoveFoundation Bool
  | Undo
  | Restart

type alias Msg = List OneMsg

newGameCmd : Cmd Msg
newGameCmd = Random.generate (List.singleton << NewGame << gameOfDeck) genDeck

appendGame : Game -> Model -> Model
appendGame updated model =
  { model
  | history =
      case model.history of
        [] -> [(updated, [])]
        (now, past) :: rest ->
          (updated, now :: past) :: rest
  }

updateGame : (Game -> Maybe Game) -> Model -> Maybe (Model, Cmd Msg)
updateGame f model =
  case model.history of
    [] -> Nothing
    (game, _) :: _ ->
      f game
      |> Maybe.map (\newGame ->
          ( appendGame newGame model
          , Cmd.batch [setTouchConfig newGame, autoMove model newGame]
          )
        )

allSources : Game -> List (FromLocation, Card)
allSources game =
  [ Array.toList game.foundations
    |> List.indexedMap
        (\i c -> if c.rank > 0 then Just (FromFoundation i, c) else Nothing)
  , Array.toList game.freeCells
    |> List.indexedMap
       (\i mc -> mc |> Maybe.map (\c -> (FromFreeCell i, c)))
  , Array.toList (Array.map List.head game.cascades)
    |> List.indexedMap
       (\i mc -> mc |> Maybe.map (\c -> (FromCascade i 1, c)))
  ] |> List.concat |> List.filterMap identity

autoMove : Model -> Game -> Cmd Msg
autoMove model game =
  if not model.autoMoveFoundation
  then Cmd.none
  else
    let
      nextRank foundations =
        foundations
        |> List.map .rank
        |> List.minimum
        |> Maybe.withDefault 0
        |> \current -> current + 1
      (nextRed, nextBlack) =
        List.partition (\c -> isRed c.suit) (Array.toList game.foundations)
        |> \(rf, bf) -> (nextRank rf, nextRank bf)
      canAuto card =
        List.any (\f -> card.suit == f.suit && card.rank == f.rank + 1)
          (Array.toList game.foundations)
      shouldAuto card = card.rank <= 1 + if isRed card.suit then nextBlack else nextRed
      tryAuto (src, card) =
        if canAuto card && shouldAuto card
        then [TryMove src ToFoundation]
        else []
    in
    List.concatMap tryAuto (allSources game)
    |> Task.succeed >> Task.perform identity

init : () -> (Model, Cmd Msg)
init () =
  ( { history = []
    , errors = []
    , drag = Drag.init
    , highlightSeq = True
    , highlightFoundation = True
    , autoMoveFoundation = False
    }
  , newGameCmd
  )

removeFromSource : FromLocation -> Game -> Game
removeFromSource src game =
  case src of
    FromFoundation i ->
      case Array.get i game.foundations of
        Nothing -> game
        Just f ->
          { game
          | foundations = Array.set i { f | rank = f.rank - 1 } game.foundations
          }
    FromFreeCell i -> { game | freeCells = Array.set i Nothing game.freeCells }
    FromCascade i count ->
      case Array.get i game.cascades of
        Nothing -> game
        Just cards ->
          { game | cascades = Array.set i (List.drop count cards) game.cascades }

-- should count empty cascades too
numEmptyFreeCells : Game -> Int
numEmptyFreeCells game =
  let
    f fc acc =
      case fc of
        Just _ -> acc
        Nothing -> acc + 1
  in
  Array.foldl f 0 game.freeCells

tryMove : FromLocation -> DropLocation -> Game -> Maybe Game
tryMove src dst game =
  let
    moveCards = cardsFromSource game src
    topCard = List.foldl (always << Just) Nothing moveCards
  in
  if dropLocation src == dst || List.length moveCards > numEmptyFreeCells game + 1
  then Nothing
  else
    case dst of
      ToFoundation ->
        let
          tryFoundation i card maybeUpdated =
            case Array.get i game.foundations of
              Nothing -> Nothing
              Just f ->
                if card.suit == f.suit && card.rank == f.rank + 1
                then
                  Maybe.withDefault game maybeUpdated
                  |> (\g -> { g | foundations = Array.set i card g.foundations })
                  |> Just
                else tryFoundation (i + 1) card maybeUpdated
        in
        -- I'm not certain this tries the cards in the "right" order, but
        -- it shouldn't matter since adjacent cards of the same suit can't
        -- be part of a moveable stack.
        List.foldl (tryFoundation 0) Nothing moveCards
        |> Maybe.map (removeFromSource src)
      ToFreeCell i ->
        case (Array.get i game.freeCells, moveCards) of
          (Just Nothing, [card]) ->
            { game
            | freeCells = Array.set i (Just card) game.freeCells
            } |> removeFromSource src
              |> Just
          _ -> Nothing
      ToCascade i ->
        case (Array.get i game.cascades, topCard) of
          (_, Nothing) -> Nothing
          (Just cascade, Just srcLink) ->
            let
              compatible =
                case List.head cascade of
                  Nothing -> True
                  Just dstLink -> sequenceCompatible srcLink dstLink
            in
            if compatible
            then
              { game
              | cascades = Array.set i (moveCards ++ cascade) game.cascades
              } |> removeFromSource src
                |> Just
            else Nothing
          (Nothing, Just _) -> Nothing

type Location
  = Foundation (Maybe Int)
  | FreeCell Int
  | Cascade Int (Maybe Int)

ofFrom : FromLocation -> Location
ofFrom from =
  case from of
    FromFoundation i -> Foundation (Just i)
    FromFreeCell i -> FreeCell i
    FromCascade i c -> Cascade i (Just c)

ofDrop : DropLocation -> Location
ofDrop drop =
  case drop of
    ToFoundation -> Foundation Nothing
    ToFreeCell i -> FreeCell i
    ToCascade i -> Cascade i Nothing

idForLocation : Location -> String
idForLocation loc =
  case loc of
    Foundation Nothing -> "fo"
    Foundation (Just i) -> "fo" ++ String.fromInt i
    FreeCell i -> "fc" ++ String.fromInt i
    Cascade i Nothing -> "c" ++ String.fromInt i
    Cascade i (Just c) -> "c" ++ String.fromInt i ++ "-" ++ String.fromInt c

allDropLocations : Game -> List DropLocation
allDropLocations game =
  [ [ ToFoundation ]
  , List.indexedMap (\i _ -> ToFreeCell i) (Array.toList game.freeCells)
  , List.indexedMap (\i _ -> ToCascade i) (Array.toList game.cascades)
  ] |> List.concat

setTouchConfig : Game -> Cmd Msg
setTouchConfig game =
  let
    allTargetIds =
      List.map (\l -> (idForLocation (ofDrop l), l)) (allDropLocations game)
  in
  Drag.setTouchConfig { allTargetIds = allTargetIds }
  |> Cmd.map (List.singleton << Drag)

updateOne : OneMsg -> Model -> (Model, Cmd Msg)
updateOne msg model =
  case msg of
    AddError new -> ({ model | errors = new :: model.errors }, Cmd.none)
    NewGame game ->
      ( { model | history = (game, []) :: model.history }
      , setTouchConfig game
      )
    AppendGame game ->
      ( appendGame game model
      , setTouchConfig game
      )
    RequestNewGame -> (model, newGameCmd)
    Drag dragMsg ->
      ( { model | drag = Drag.update dragMsg model.drag }
      , case (Drag.held model.drag, dragMsg) of
          (Just held, Drag.Drop (Just target)) ->
            Task.perform identity (Task.succeed [TryMove held target])
          _ -> Cmd.none
      )
    TryMove from to ->
      updateGame (tryMove from to) model
      |> Maybe.withDefault (model, Cmd.none)
    SetHighlightSeq to -> ({ model | highlightSeq = to }, Cmd.none)
    SetHighlightFoundation to -> ({ model | highlightFoundation = to }, Cmd.none)
    SetAutoMoveFoundation to -> ({ model | autoMoveFoundation = to }, Cmd.none)
    Undo ->
      ( { model
        | history = case model.history of
            [] -> model.history
            [(_, [])] -> model.history
            (_, []) :: rest -> rest
            (_, prev :: past) :: rest ->
              (prev, past) :: rest
        }
      , Cmd.none
      )
    Restart ->
      ( { model
        | history = case model.history of
            [] -> model.history
            [(_, [])] -> model.history
            (now, past) :: rest ->
              (List.foldl (\x a -> x) now past, [])
              :: model.history
        }
      , Cmd.none
      )

update : Msg -> Model -> (Model, Cmd Msg)
update ones originalModel =
  let
    doOne one (model, cmdsSoFar) =
      let
        (updatedModel, newCmd) = updateOne one model
      in
      (updatedModel, newCmd :: cmdsSoFar)
    (finalModel, cmds) = List.foldl doOne (originalModel, []) ones
  in
  (finalModel, Cmd.batch cmds)
