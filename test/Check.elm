port module Check exposing (main)

import Array
import Json.Encode
import Platform

import Drag
import Model

-- Checks that run the real Model.updateOne. Each one is here because a
-- plausible change could quietly break it, not to restate what the types
-- already say.

port report : List { name : String, ok : Bool } -> Cmd msg

card : Model.Suit -> Int -> Model.Card
card suit rank = { suit = suit, rank = rank }

-- one move from won: the king of clubs in a free cell, everything else home
nearlyWon : Model.Table
nearlyWon =
  { foundations =
      Array.fromList
        [ card Model.Spades 13
        , card Model.Hearts 13
        , card Model.Diamonds 13
        , card Model.Clubs 12
        ]
  , freeCells =
      Array.fromList [ Just (card Model.Clubs 13), Nothing, Nothing, Nothing ]
  , cascades = Array.repeat 8 []
  }

-- record update syntax wants a plain name
emptyTable : Model.Table
emptyTable = Model.emptyTable

-- a run of three with somewhere to go and two free cells to get it there
runOfThree : Model.Table
runOfThree =
  { emptyTable
  | freeCells =
      Array.fromList
        [ Nothing
        , Nothing
        , Just (card Model.Spades 9)
        , Just (card Model.Hearts 10)
        ]
  , cascades =
      Array.repeat 8 []
      |> Array.set 0
          [ card Model.Clubs 4, card Model.Hearts 5, card Model.Spades 6 ]
      |> Array.set 1 [ card Model.Diamonds 7 ]
  }

toCascade1 : Model.Grab -> Model.Table -> Maybe (List Model.Table)
toCascade1 from = Model.performMove from (Model.ToCascade 1)

endsAt : Maybe (List Model.Table) -> Maybe Model.Table
endsAt = Maybe.andThen (List.foldl (\table _ -> Just table) Nothing)

cascade : Int -> Model.Table -> List Model.Card
cascade i table = Array.get i table.cascades |> Maybe.withDefault []

freeCell : Int -> Model.Table -> Maybe Model.Card
freeCell i table = Array.get i table.freeCells |> Maybe.andThen identity

-- an empty table if there's no game, which no check should be happy with
tableOf : Model.Model -> Model.Table
tableOf = Model.playing >> Maybe.map .now >> Maybe.withDefault emptyTable

-- playing one particular game, having got as far as the given table
atGame : { firstUnsolved : Int, seed : Int } -> Model.Table -> Model.Model
atGame { firstUnsolved, seed } table =
  { errors = []
  , history =
      Just
        { current = { seed = seed, now = table, past = [] }
        , previous = []
        }
  , drag = Drag.init
  , highlightSeq = True
  , highlightFoundation = True
  , autoMove = { lowFoundation = False }
  , firstUnsolved = Just firstUnsolved
  , seedInput = String.fromInt seed
  }

playTheKing : Model.Model -> Model.Model
playTheKing =
  Model.updateOne (Model.TryMove (Model.FromFreeCell 0) Model.ToFoundation)
  >> Tuple.first

newGame : Model.Model -> Model.Model
newGame = Model.updateOne Model.RequestNewGame >> Tuple.first

undo : Model.Model -> Model.Model
undo = Model.updateOne Model.Undo >> Tuple.first

restart : Model.Model -> Model.Model
restart = Model.updateOne Model.Restart >> Tuple.first

seedOf : Model.Model -> Maybe Int
seedOf = Model.playing >> Maybe.map .seed

wonNow : Model.Model -> Bool
wonNow = Model.playing >> Maybe.map (.now >> Model.isWon) >> Maybe.withDefault False

stored : String -> Model.Model
stored contents = Model.init (Json.Encode.string contents) |> Tuple.first

checks : List { name : String, ok : Bool }
checks =
  let
    upTo5 = atGame { firstUnsolved = 5, seed = 5 } nearlyWon
    won = playTheKing upTo5
    unreadable = stored "hello"
    recovered = newGame { unreadable | seedInput = "42" }
  in
  -- One number can stand in for a set of solved games only because the games
  -- you beat out of order don't count, so pin both halves of that.
  [ { name = "winning the game you're up to moves you on"
    , ok = won.firstUnsolved == Just 6
    }
  , { name = "winning one you'd already beaten leaves it alone"
    , ok =
        (playTheKing (atGame { firstUnsolved = 9, seed = 5 } nearlyWon)).firstUnsolved
          == Just 9
    }
  , { name = "winning one ahead of it leaves it alone"
    , ok =
        (playTheKing (atGame { firstUnsolved = 3, seed = 5 } nearlyWon)).firstUnsolved
          == Just 3
    }
  -- what "new game" means depends on the game you're on, which is easy to
  -- lose track of in a rewrite of RequestNewGame
  , { name = "new game after a win deals the next one"
    , ok = seedOf (newGame won) == Just 6
    }
  , { name = "new game without a win deals the same one again"
    , ok = seedOf (newGame upTo5) == Just 5
    }
  , { name = "a number you type beats both"
    , ok = seedOf (newGame { won | seedInput = "12" }) == Just 12
    }
  -- the two halves of storage have to agree, and only meet in a browser
  , { name = "what we save is what we can read back"
    , ok =
        Model.encodeProgress 6
        |> Json.Encode.encode 0
        |> Json.Encode.string
        |> Model.storedFirstUnsolved
        |> Result.toMaybe
        |> (==) (Just 6)
    }
  -- defaulting to game 0 here instead would silently throw real progress away
  , { name = "storage we can't read deals no game and guesses nothing"
    , ok =
        unreadable.history == Nothing
          && unreadable.firstUnsolved == Nothing
          && List.length unreadable.errors == 1
    }
  , { name = "and the game you pick then becomes what we remember"
    , ok = recovered.firstUnsolved == Just 42 && seedOf recovered == Just 42
    }
  -- keeping the games apart in the history is what these two need
  , { name = "restart goes back to the deal, and undo takes the restart back"
    , ok =
        not (wonNow (restart won)) && wonNow (undo (restart won))
    }
  -- a run of cards moving is really the free cells filling up and emptying
  -- again, and we'd rather show that than pretend the run moved at once
  , { name = "moving three cards sends two of them through the free cells"
    , ok =
        Model.planMove (Model.FromCascade 0 3) (Model.ToCascade 1) runOfThree
          == Just
              [ { from = Model.FromCascade 0 Model.One, to = Model.ToFreeCell 0 }
              , { from = Model.FromCascade 0 Model.One, to = Model.ToFreeCell 1 }
              , { from = Model.FromCascade 0 Model.One, to = Model.ToCascade 1 }
              , { from = Model.FromFreeCell 1, to = Model.ToCascade 1 }
              , { from = Model.FromFreeCell 0, to = Model.ToCascade 1 }
              ]
    }
  , { name = "and there has to be a cell free for all but one of them"
    , ok =
        { runOfThree
        | freeCells =
            Array.set 1 (Just (card Model.Clubs 9)) runOfThree.freeCells
        }
        |> Model.planMove (Model.FromCascade 0 3) (Model.ToCascade 1)
        |> (==) Nothing
    }
  , { name = "the cards arrive as if they had moved all at once"
    , ok =
        toCascade1 (Model.FromCascade 0 3) runOfThree
        |> endsAt
        |> Maybe.map
            (\table ->
              cascade 1 table
                == [ card Model.Clubs 4
                   , card Model.Hearts 5
                   , card Model.Spades 6
                   , card Model.Diamonds 7
                   ]
                && cascade 0 table == []
                && table.freeCells == runOfThree.freeCells
            )
        |> Maybe.withDefault False
    }
  -- the commonest move in the game, and the only check where the cascade a
  -- card comes from isn't cascade 0
  , { name = "one card moves from one cascade to another"
    , ok =
        let
          twoCascades =
            { emptyTable
            | cascades =
                Array.repeat 8 []
                |> Array.set 2 [ card Model.Hearts 5 ]
                |> Array.set 1 [ card Model.Spades 6 ]
            }
        in
        toCascade1 (Model.FromCascade 2 1) twoCascades
        |> endsAt
        |> Maybe.map
            (\table ->
              cascade 1 table == [ card Model.Hearts 5, card Model.Spades 6 ]
                && cascade 2 table == []
            )
        |> Maybe.withDefault False
    }
  -- a plan is a proposal: every step of it still has to be a legal move, which
  -- is what stops a drag of cards that aren't a sequence
  , { name = "cards that aren't a sequence don't move, one at a time or at all"
    , ok =
        let
          notARun =
            { emptyTable
            | cascades =
                Array.repeat 8 []
                |> Array.set 0 [ card Model.Hearts 3, card Model.Diamonds 9 ]
                |> Array.set 1 [ card Model.Clubs 10 ]
            }
        in
        Model.planMove (Model.FromCascade 0 2) (Model.ToCascade 1) notARun
          /= Nothing
          && toCascade1 (Model.FromCascade 0 2) notARun == Nothing
    }
  -- cards go to the foundations one at a time, so unlike a run moving between
  -- cascades this needs no free cells at all
  , { name = "a run can go to the foundations with every cell full"
    , ok =
        let
          onePlayed =
            { emptyTable
            | foundations =
                Array.set 1 (card Model.Hearts 1) emptyTable.foundations
            , freeCells = Array.repeat 4 (Just (card Model.Spades 9))
            , cascades =
                Array.repeat 8 []
                |> Array.set 0 [ card Model.Spades 1, card Model.Hearts 2 ]
            }
        in
        Model.performMove (Model.FromCascade 0 2) Model.ToFoundation onePlayed
        |> endsAt
        |> Maybe.map
            (\table ->
              table.foundations
                == Array.fromList
                    [ card Model.Spades 1
                    , card Model.Hearts 2
                    , card Model.Diamonds 0
                    , card Model.Clubs 0
                    ]
                && cascade 0 table == []
            )
        |> Maybe.withDefault False
    }
  -- this is the decision that a run is not one move for undo's purposes; if
  -- that changes, this check is the thing that should notice
  , { name = "undo takes the cards of a run back one at a time"
    , ok =
        let
          movedTheRun =
            atGame { firstUnsolved = 0, seed = 0 } runOfThree
            |> Model.updateOne
                (Model.TryMove (Model.FromCascade 0 3) (Model.ToCascade 1))
            |> Tuple.first
          arrived = tableOf movedTheRun
          steppedBack = tableOf (undo movedTheRun)
        in
        List.length (cascade 1 arrived) == 4
          && List.length (cascade 1 steppedBack) == 3
          && freeCell 0 steppedBack == Just (card Model.Clubs 4)
    }
  -- a foundation only remembers its top card, so taking one back means the
  -- one beneath it reappears by counting down
  , { name = "a card comes back off a foundation, and the one beneath shows"
    , ok =
        let
          twoPlayed =
            { emptyTable
            | foundations =
                Array.set 0 (card Model.Spades 2) emptyTable.foundations
            }
        in
        Model.performMove (Model.FromFoundation 0) (Model.ToFreeCell 0) twoPlayed
        |> endsAt
        |> Maybe.map
            (\table ->
              freeCell 0 table == Just (card Model.Spades 2)
                && Array.get 0 table.foundations == Just (card Model.Spades 1)
            )
        |> Maybe.withDefault False
    }
  , { name = "and there's nothing to take off an empty one"
    , ok =
        Model.performMove (Model.FromFoundation 2) (Model.ToFreeCell 0) emptyTable
          == Nothing
    }
  , { name = "undo crosses into the game you left, and says so in the box"
    , ok =
        let
          back = undo (newGame won)
        in
        seedOf back == Just 5 && back.seedInput == "5"
    }
  ]

main : Program () () ()
main =
  Platform.worker
    { init = \_ -> ((), report checks)
    , update = \_ model -> (model, Cmd.none)
    , subscriptions = \_ -> Sub.none
    }
