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
