module Model exposing (..)

import Array exposing (Array)
import Random

type Suit
  = Spades
  | Hearts
  | Diamonds
  | Clubs

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

type alias Model =
  { game : Game
  , errors : List String
  }

gameOfDeck : List Card -> Game
gameOfDeck cards = { emptyGame | cascades = cascadesOfDeck 8 cards }

type OneMsg
  = RequestNewGame
  | SetGame Game
  | AddError String

type alias Msg = List OneMsg

newGameCmd : Cmd Msg
newGameCmd = Random.generate (List.singleton << SetGame << gameOfDeck) genDeck

init : () -> (Model, Cmd Msg)
init () =
  ( { game = emptyGame, errors = [] }
  , newGameCmd
  )

updateOne : OneMsg -> Model -> (Model, Cmd Msg)
updateOne msg model =
  case msg of
    SetGame game -> ({ model | game = game }, Cmd.none)
    RequestNewGame -> (model, newGameCmd)
    AddError new -> ({ model | errors = new :: model.errors }, Cmd.none)

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
