port module Model exposing (..)

import Array exposing (Array)
import Json.Decode
import Json.Encode
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

-- everything on the table at one moment
type alias Table =
  { foundations : Array Card
  , freeCells : Array (Maybe Card)
  , cascades : Array (List Card)
  }

emptyTable : Table
emptyTable =
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

-- Where cards are coming from. The count of them belongs to the cascade case
-- because a cascade is the only place you can take more than one from, and it
-- is a parameter so that a move of exactly one card can say so in its type.
type FromLocation count
  = FromFoundation Int
  | FromFreeCell Int
  | FromCascade Int count

-- a count of one, and no other count
type One = One

-- what the player took hold of: somewhere, and how many cards of it
type alias Grab = FromLocation Int

-- where a single card is
type alias Single = FromLocation One

type DropLocation
  = ToFoundation
  | ToFreeCell Int
  | ToCascade Int

dropLocation : FromLocation count -> DropLocation
dropLocation src =
  case src of
    FromFoundation _ -> ToFoundation
    FromFreeCell i -> ToFreeCell i
    FromCascade i _ -> ToCascade i

type alias AutoMove =
  { lowFoundation : Bool
  }

-- A game is a deal, named by the seed it came from, and the positions we've
-- moved through since, most recent first.
type alias Game =
  { seed : Int
  , now : Table
  , past : List Table
  }

-- The game we're playing and the ones we dealt before it. Keeping the games
-- apart rather than in one run of positions is what lets restart find the deal
-- to go back to; undo crossing the join into the game underneath comes free.
type alias History =
  { current : Game
  , previous : List Game
  }

type alias Model =
  { errors : List String
  -- Nothing until we've dealt a game, which so far means only while we're
  -- asking the player where they'd got to
  , history : Maybe History
  , drag : Drag.Model Grab DropLocation
  , highlightSeq : Bool
  , highlightFoundation : Bool
  , autoMove : AutoMove
  -- the number of the lowest game the player hasn't beaten: it's where they
  -- pick up when they come back, and it's all we remember about them.
  -- Nothing means we couldn't read what we'd stored, and are waiting to be
  -- told what to replace it with.
  , firstUnsolved : Maybe Int
  -- what's in the text box, which is only the game we're playing until it's
  -- typed in
  , seedInput : String
  }

port setProgress : Json.Encode.Value -> Cmd msg

-- An object with one field so far, so that remembering something else later
-- doesn't have to mean reading the old shape back.
progressDecoder : Json.Decode.Decoder Int
progressDecoder = Json.Decode.field "firstUnsolved" Json.Decode.int

encodeProgress : Int -> Json.Encode.Value
encodeProgress firstUnsolved =
  Json.Encode.object [("firstUnsolved", Json.Encode.int firstUnsolved)]

-- Knowing where the player has got to and remembering it are the same thing.
setFirstUnsolved : Int -> Model -> (Model, Cmd Msg)
setFirstUnsolved firstUnsolved model =
  ( { model | firstUnsolved = Just firstUnsolved }
  , setProgress (encodeProgress firstUnsolved)
  )

tableOfDeck : List Card -> Table
tableOfDeck cards = { emptyTable | cascades = cascadesOfDeck 8 cards }

type alias DragMsg = Drag.Msg Grab DropLocation

-- Everywhere but a cascade holds one card at most, so those cases are asking
-- cardFromSource the same question a step does.
cardsFromSource : Table -> Grab -> List Card
cardsFromSource table grab =
  let
    justTheOne src =
      cardFromSource table src
      |> Maybe.map List.singleton
      |> Maybe.withDefault []
  in
  case grab of
    FromFoundation i -> justTheOne (FromFoundation i)
    FromFreeCell i -> justTheOne (FromFreeCell i)
    FromCascade i count ->
      case Array.get i table.cascades of
        Nothing -> []
        Just cards -> List.take count cards

type OneMsg
  = AddError String
  | RequestNewGame
  | NewGameSeed Int
  | AppendTable Table
  | Drag DragMsg
  | TryMove Grab DropLocation
  | SetHighlightSeq Bool
  | SetHighlightFoundation Bool
  | SetAutoMoveFoundation Bool
  | SetSeedInput String
  | Undo
  | Restart

type alias Msg = List OneMsg

-- The game number is the seed: game 0 is the same deal for everyone, forever.
gameOfSeed : Int -> Game
gameOfSeed seed =
  { seed = seed
  , now =
      Random.step genDeck (Random.initialSeed seed)
      |> Tuple.first
      |> tableOfDeck
  , past = []
  }

isWon : Table -> Bool
isWon table =
  Array.toList table.foundations |> List.all (\card -> card.rank == 13)

playing : Model -> Maybe Game
playing model = Maybe.map .current model.history

-- the box shows the game you're looking at, until you type in it
showSeed : Model -> Model
showSeed model =
  case playing model of
    Nothing -> model
    Just current -> { model | seedInput = String.fromInt current.seed }

-- a freshly dealt game, with whatever we were playing left underneath it
startGame : Game -> Maybe History -> History
startGame dealt history =
  { current = dealt
  , previous =
      case history of
        Nothing -> []
        Just old -> old.current :: old.previous
  }

-- back one position, or if there aren't any, back to the game we dealt before
undoHistory : History -> History
undoHistory history =
  let
    current = history.current
  in
  case (current.past, history.previous) of
    (prev :: older, _) ->
      { history | current = { current | now = prev, past = older } }
    ([], []) -> history
    ([], earlier :: rest) -> { current = earlier, previous = rest }

-- back to the deal, keeping where we'd got to so that undo can take it back
restartHistory : History -> History
restartHistory history =
  let
    current = history.current
  in
  case current.past of
    [] -> history
    _ ->
      { current =
          { current
          | now = List.foldl (\x a -> x) current.now current.past
          , past = []
          }
      , previous = current :: history.previous
      }

-- the next position in the game we're playing
appendTable : Table -> History -> History
appendTable updated history =
  let
    current = history.current
  in
  { history
  | current = { current | now = updated, past = current.now :: current.past }
  }

-- One thing the player did, which may be several positions: each is kept, so
-- undo takes the cards back one at a time. Making undo treat the run as one
-- move would be a matter of appending it as one rather than folding it in.
updateTables : (Table -> Maybe (List Table)) -> Model -> Maybe (Model, Cmd Msg)
updateTables f model =
  model.history
  |> Maybe.andThen (\history ->
      let
        current = history.current
      in
      f current.now
      |> Maybe.andThen (\tables ->
          case tables of
            [] -> Nothing
            _ ->
              let
                moved = List.foldl appendTable history tables
                ended = moved.current.now
                -- Beating a game we've already beaten, or one beyond the first
                -- we haven't, tells us nothing we aren't already remembering.
                (progressed, saveProgress) =
                  if isWon ended && model.firstUnsolved == Just current.seed
                  then setFirstUnsolved (current.seed + 1) model
                  else (model, Cmd.none)
              in
              Just
                ( { progressed | history = Just moved }
                , Cmd.batch
                    [ setTouchConfig ended
                    , autoMove model ended
                    , saveProgress
                    ]
                )
        )
    )

allSources : Table -> List (Grab, Card)
allSources table =
  [ Array.toList table.foundations
    |> List.indexedMap
        (\i c -> if c.rank > 0 then Just (FromFoundation i, c) else Nothing)
  , Array.toList table.freeCells
    |> List.indexedMap
       (\i mc -> mc |> Maybe.map (\c -> (FromFreeCell i, c)))
  , Array.toList (Array.map List.head table.cascades)
    |> List.indexedMap
       (\i mc -> mc |> Maybe.map (\c -> (FromCascade i 1, c)))
  ] |> List.concat |> List.filterMap identity

autoMove : Model -> Table -> Cmd Msg
autoMove model table =
  if not model.autoMove.lowFoundation
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
        List.partition (\c -> isRed c.suit) (Array.toList table.foundations)
        |> \(rf, bf) -> (nextRank rf, nextRank bf)
      canAuto card =
        List.any (\f -> card.suit == f.suit && card.rank == f.rank + 1)
          (Array.toList table.foundations)
      shouldAuto card =
        card.rank <= min
          (1 + if isRed card.suit then nextBlack else nextRed)
          (2 + if isRed card.suit then nextRed else nextBlack)
      tryAuto (src, card) =
        if canAuto card && shouldAuto card
        then [TryMove src ToFoundation]
        else []
    in
    List.concatMap tryAuto (allSources table)
    |> Task.succeed >> Task.perform identity

-- The flags are whatever was in storage, as a string, or null for a first
-- visit, which starts at the beginning.
storedFirstUnsolved : Json.Decode.Value -> Result Json.Decode.Error Int
storedFirstUnsolved flags =
  case Json.Decode.decodeValue (Json.Decode.nullable Json.Decode.string) flags of
    Err err -> Err err
    Ok Nothing -> Ok 0
    Ok (Just stored) -> Json.Decode.decodeString progressDecoder stored

init : Json.Decode.Value -> (Model, Cmd Msg)
init flags =
  let
    noGame =
      { history = Nothing
      , errors = []
      , drag = Drag.init
      , highlightSeq = True
      , highlightFoundation = True
      , autoMove = { lowFoundation = True }
      , firstUnsolved = Nothing
      , seedInput = ""
      }
  in
  case storedFirstUnsolved flags of
    Ok firstUnsolved ->
      updateOne
        (NewGameSeed firstUnsolved)
        { noGame | firstUnsolved = Just firstUnsolved }
    -- Storage we can't read is more likely a migration we forgot to write than
    -- anything else, so don't deal a game and don't overwrite it: say what we
    -- found, and let the player say where they'd got to.
    Err err ->
      ( { noGame
        | errors =
            [ "couldn't read your progress: " ++ Json.Decode.errorToString err ]
        }
      , Cmd.none
      )

-- the one card a step picks up
cardFromSource : Table -> Single -> Maybe Card
cardFromSource table src =
  case src of
    FromFoundation i ->
      Array.get i table.foundations
      |> Maybe.andThen (\card -> if card.rank == 0 then Nothing else Just card)
    FromFreeCell i -> Array.get i table.freeCells |> Maybe.andThen identity
    FromCascade i One -> Array.get i table.cascades |> Maybe.andThen List.head

removeFromSource : Single -> Table -> Table
removeFromSource src table =
  case src of
    FromFoundation i ->
      case Array.get i table.foundations of
        Nothing -> table
        Just f ->
          { table
          | foundations = Array.set i { f | rank = f.rank - 1 } table.foundations
          }
    FromFreeCell i -> { table | freeCells = Array.set i Nothing table.freeCells }
    FromCascade i One ->
      case Array.get i table.cascades of
        Nothing -> table
        Just cards ->
          { table | cascades = Array.set i (List.drop 1 cards) table.cascades }

emptyFreeCells : Table -> List Int
emptyFreeCells table =
  Array.toIndexedList table.freeCells
  |> List.filterMap (\(i, card) -> if card == Nothing then Just i else Nothing)

-- the grab a step makes: whatever the player took hold of, one card of it
oneOf : Grab -> Single
oneOf from =
  case from of
    FromFoundation i -> FromFoundation i
    FromFreeCell i -> FromFreeCell i
    FromCascade i _ -> FromCascade i One

-- One card going from somewhere to somewhere: the only kind of move that
-- really happens.
type alias Step =
  { from : Single
  , to : DropLocation
  }

-- What the player asks for isn't always a single move. Moving a run of cards
-- to another cascade means sending all but the last out to the free cells and
-- fetching them back, and dropping a run on the foundations means playing the
-- cards one at a time. Working out those steps rather than lifting the run in
-- one go is what makes the free cells' part in it something we can show, check
-- and take back.
--
-- Just means we found a way to break the move up, not that the way works:
-- whether each step of it is legal is runPlan's business.
--
-- Empty cascades could hold cards on the way too, which would make longer runs
-- moveable than this manages.
planMove : Grab -> DropLocation -> Table -> Maybe (List Step)
planMove from to table =
  let
    cards = cardsFromSource table from
    -- moving a run of n cards needs somewhere to put n - 1 of them
    viaFreeCells i =
      let
        cells = emptyFreeCells table |> List.take (List.length cards - 1)
      in
      if List.length cells < List.length cards - 1
      then Nothing
      else
        let
          stash cell = { from = FromCascade i One, to = ToFreeCell cell }
          fetch cell = { from = FromFreeCell cell, to = to }
          moveTheLast = { from = FromCascade i One, to = to }
        in
        Just
          (List.map stash cells
            ++ moveTheLast
            :: List.map fetch (List.reverse cells)
          )
  in
  if dropLocation from == to
  then Nothing
  else
    case (cards, from, to) of
      ([], _, _) -> Nothing
      ([_], _, _) -> Just [{ from = oneOf from, to = to }]
      (_, FromCascade i _, ToCascade _) -> viaFreeCells i
      (_, FromCascade i _, ToFoundation) ->
        Just (List.map (always { from = FromCascade i One, to = to }) cards)
      _ -> Nothing

-- The positions the table passes through, in order, or nothing if a step turns
-- out to be illegal after all: a plan is a proposal, not a promise.
runPlan : List Step -> Table -> Maybe (List Table)
runPlan steps table =
  case steps of
    [] -> Just []
    step :: rest ->
      tryMove step.from step.to table
      |> Maybe.andThen (\stepped ->
          runPlan rest stepped |> Maybe.map ((::) stepped)
        )

-- everything a move the player asked for passes through
performMove : Grab -> DropLocation -> Table -> Maybe (List Table)
performMove from to table =
  planMove from to table |> Maybe.andThen (\steps -> runPlan steps table)

tryMove : Single -> DropLocation -> Table -> Maybe Table
tryMove src dst table =
  if dropLocation src == dst
  then Nothing
  else
    cardFromSource table src
    |> Maybe.andThen (\card ->
        case dst of
          ToFoundation ->
            Array.toIndexedList table.foundations
            |> List.filter
                (\(_, f) -> card.suit == f.suit && card.rank == f.rank + 1)
            |> List.head
            |> Maybe.map
                (\(i, _) ->
                  { table | foundations = Array.set i card table.foundations }
                )
          ToFreeCell i ->
            case Array.get i table.freeCells of
              Just Nothing ->
                Just
                  { table | freeCells = Array.set i (Just card) table.freeCells }
              _ -> Nothing
          ToCascade i ->
            Array.get i table.cascades
            |> Maybe.andThen
                (\cascade ->
                  if
                    List.head cascade
                    |> Maybe.map (sequenceCompatible card)
                    |> Maybe.withDefault True
                  then
                    Just
                      { table
                      | cascades = Array.set i (card :: cascade) table.cascades
                      }
                  else Nothing
                )
      )
    |> Maybe.map (removeFromSource src)

type Location
  = Foundation (Maybe Int)
  | FreeCell Int
  | Cascade Int (Maybe Int)

-- the count is which card of the cascade, which is how it gets its id
ofFrom : Grab -> Location
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

allDropLocations : Table -> List DropLocation
allDropLocations table =
  [ [ ToFoundation ]
  , List.indexedMap (\i _ -> ToFreeCell i) (Array.toList table.freeCells)
  , List.indexedMap (\i _ -> ToCascade i) (Array.toList table.cascades)
  ] |> List.concat

setTouchConfig : Table -> Cmd Msg
setTouchConfig table =
  let
    allTargetIds =
      List.map (\l -> (idForLocation (ofDrop l), l)) (allDropLocations table)
  in
  Drag.setTouchConfig { allTargetIds = allTargetIds }
  |> Cmd.map (List.singleton << Drag)

updateOne : OneMsg -> Model -> (Model, Cmd Msg)
updateOne msg model =
  case msg of
    AddError new -> ({ model | errors = new :: model.errors }, Cmd.none)
    NewGameSeed seed ->
      let
        dealt = gameOfSeed seed
        -- If we couldn't read where the player had got to, the game they pick
        -- is our new answer.
        (based, saveProgress) =
          case model.firstUnsolved of
            Just _ -> (model, Cmd.none)
            Nothing -> setFirstUnsolved seed model
      in
      ( showSeed { based | history = Just (startGame dealt based.history) }
      , Cmd.batch [setTouchConfig dealt.now, saveProgress]
      )
    AppendTable table ->
      ( { model | history = Maybe.map (appendTable table) model.history }
      , setTouchConfig table
      )
    RequestNewGame ->
      case String.toInt (String.trim model.seedInput) of
        Nothing ->
          updateOne
            (AddError (model.seedInput ++ " is not a game number"))
            model
        Just seed ->
          -- Once you've won, asking for a new game means the next one, not
          -- another go at the one you've just beaten.
          let
            wonThisOne =
              playing model
              |> Maybe.map (\current ->
                  current.seed == seed && isWon current.now
                )
              |> Maybe.withDefault False
          in
          updateOne
            (NewGameSeed (if wonThisOne then seed + 1 else seed))
            model
    Drag dragMsg ->
      ( { model | drag = Drag.update dragMsg model.drag }
      , case (Drag.held model.drag, dragMsg) of
          (Just held, Drag.Drop (Just target)) ->
            Task.perform identity (Task.succeed [TryMove held target])
          _ -> Cmd.none
      )
    TryMove from to ->
      updateTables (performMove from to) model
      |> Maybe.withDefault (model, Cmd.none)
    SetHighlightSeq to -> ({ model | highlightSeq = to }, Cmd.none)
    SetHighlightFoundation to -> ({ model | highlightFoundation = to }, Cmd.none)
    SetAutoMoveFoundation to -> ({ model | autoMove = { lowFoundation = to } }, Cmd.none)
    SetSeedInput to -> ({ model | seedInput = to }, Cmd.none)
    Undo ->
      ( showSeed { model | history = Maybe.map undoHistory model.history }
      , Cmd.none
      )
    Restart ->
      ( { model | history = Maybe.map restartHistory model.history }
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
