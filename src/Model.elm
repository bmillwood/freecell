module Model exposing (..)

import Array exposing (Array)
import Random

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

type CardLocation
  = Foundation Int
  | FreeCell Int
  | Cascade Int Int

type DropLocation
  = ToFoundation
  | ToFreeCell Int
  | ToCascade Int

dropLocation : CardLocation -> DropLocation
dropLocation src =
  case src of
    Foundation _ -> ToFoundation
    FreeCell i -> ToFreeCell i
    Cascade i _ -> ToCascade i

type alias Model =
  { errors : List String
  , history : List (Game, List Game)
  , drag : Drag.Model CardLocation DropLocation
  , highlightSeq : Bool
  , highlightFoundation : Bool
  }

gameOfDeck : List Card -> Game
gameOfDeck cards = { emptyGame | cascades = cascadesOfDeck 8 cards }

type alias DragMsg = Drag.Msg CardLocation DropLocation

cardsFromSource : Game -> CardLocation -> List Card
cardsFromSource game loc =
  case loc of
    Foundation i ->
      case Array.get i game.foundations of
        Nothing -> []
        Just card ->
          if card.rank == 0
          then []
          else [card]
    FreeCell i ->
      case Array.get i game.freeCells |> Maybe.andThen identity of
        Nothing -> []
        Just card -> [card]
    Cascade i count ->
      case Array.get i game.cascades of
        Nothing -> []
        Just cards -> List.take count cards

type OneMsg
  = AddError String
  | RequestNewGame
  | NewGame Game
  | AppendGame Game
  | Drag DragMsg
  | SetHighlightSeq Bool
  | SetHighlightFoundation Bool
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

init : () -> (Model, Cmd Msg)
init () =
  ( { history = []
    , errors = []
    , drag = Drag.init
    , highlightSeq = True
    , highlightFoundation = True
    }
  , newGameCmd
  )

removeFromSource : CardLocation -> Game -> Game
removeFromSource src game =
  case src of
    Foundation i ->
      case Array.get i game.foundations of
        Nothing -> game
        Just f ->
          { game
          | foundations = Array.set i { f | rank = f.rank - 1 } game.foundations
          }
    FreeCell i -> { game | freeCells = Array.set i Nothing game.freeCells }
    Cascade i count ->
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

tryMove : CardLocation -> DropLocation -> Game -> Maybe Game
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

idForDrop : DropLocation -> String
idForDrop loc =
  case loc of
    ToFoundation -> "fo"
    ToFreeCell i -> "fc" ++ String.fromInt i
    ToCascade i -> "c" ++ String.fromInt i

allDropLocations : Game -> List DropLocation
allDropLocations game =
  [ [ ToFoundation ]
  , List.indexedMap (\i _ -> ToFreeCell i) (Array.toList game.freeCells)
  , List.indexedMap (\i _ -> ToCascade i) (Array.toList game.cascades)
  ] |> List.concat

setTouchConfig : Game -> Cmd Msg
setTouchConfig game =
  let
    allTargetIds = List.map (\l -> (idForDrop l, l)) (allDropLocations game)
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
      let
        newGame =
          case (model.history, Drag.held model.drag, dragMsg) of
            ((game, _) :: _, Just held, Drag.Drop (Just target)) ->
              tryMove held target game
            _ -> Nothing
      in
      ( { model
        | drag = Drag.update dragMsg model.drag
        } |> case newGame of
            Nothing -> identity
            Just g -> appendGame g
      , case newGame of
          Nothing -> Cmd.none
          Just g -> setTouchConfig g
      )
    SetHighlightSeq to -> ({ model | highlightSeq = to }, Cmd.none)
    SetHighlightFoundation to -> ({ model | highlightFoundation = to }, Cmd.none)
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
