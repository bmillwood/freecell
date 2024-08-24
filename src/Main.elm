module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Model exposing (Model, Msg, Card, Suit(..))

viewSuit : Suit -> Html msg
viewSuit suit =
  let
    (className, text) =
      case suit of
        Spades -> ("spades", "♠")
        Hearts -> ("hearts", "♥")
        Diamonds -> ("diamonds", "♦")
        Clubs -> ("clubs", "♣")
  in
  Html.span [ Attributes.class className ] [ Html.text text ]

rankText : Int -> String
rankText i = case i of
  11 -> "J"
  12 -> "Q"
  13 -> "K"
  1 -> "A"
  0 -> " "
  _ -> String.fromInt i

viewCard : Card -> Html Msg
viewCard c =
  Html.span
    [ Attributes.class "card" ]
    [ Html.text (rankText c.rank), viewSuit c.suit ]

view : Model -> Browser.Document Msg
view { game, errors } =
  let
    { foundations, freeCells, cascades } = game
    viewFreeCell fc =
      case fc of
        Nothing -> Html.span [ Attributes.class "card" ] [ Html.text "\u{00A0}" ]
        Just c -> viewCard c
    viewCascade cascade =
      Html.div
        [ Attributes.class "cascade" ]
        (List.map viewCard cascade)
  in
  { title = "FreeCell"
  , body =
      [ Html.ul
          [ Attributes.class "errors" ]
          (List.map (Html.li [] << List.singleton << Html.text) errors)
      , Html.div
          [ Attributes.class "foundations" ]
          (Array.toList foundations
            |> List.map viewCard
          )
      , Html.div
          [ Attributes.class "freeCells" ]
          (Array.toList freeCells
            |> List.map viewFreeCell
          )
      , Html.div
          [ Attributes.class "cascades" ]
          (List.map viewCascade (Array.toList cascades))
      ]
  }


main = Browser.document
  { init = Model.init
  , view = view
  , update = Model.update
  , subscriptions = \model -> Sub.none
  }
