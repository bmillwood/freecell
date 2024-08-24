module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events

import Drag
import Model exposing (Model, Msg, Card, Suit(..))

nbsp : String
nbsp = "\u{00A0}"

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
  0 -> nbsp
  _ -> String.fromInt i

embedDragAttrs : List (Html.Attribute Model.DragMsg) -> List (Html.Attribute Msg)
embedDragAttrs = List.map (Attributes.map (List.singleton << Model.Drag))

view : Model -> Browser.Document Msg
view { game, errors, drag } =
  let
    { foundations, freeCells, cascades } = game
    targetAttrs loc =
      [ if Maybe.andThen .over drag == Just loc
        then [ Attributes.class "hovered" ]
        else []
      , embedDragAttrs (Drag.targetAttributes loc)
      ] |> List.concat
    targetIfOne loc =
      case drag of
        Nothing -> []
        Just { held } ->
          let (_, count) = held in
          if count == 1 then targetAttrs loc else []
    cardAttrs = [ Attributes.class "card" ]
    sourceAttrs source =
      [ [ Attributes.class "source" ]
      , embedDragAttrs (Drag.sourceAttributes source)
      ] |> List.concat
    cardContents c = [ Html.text (rankText c.rank), viewSuit c.suit ]
    viewFoundation i c =
      let
        loc = Model.Foundation i
        attrs =
          [ cardAttrs
          , if c.rank > 0 then sourceAttrs (loc, 1) else []
          , targetIfOne loc
          ] |> List.concat
      in
      Html.span attrs (cardContents c)
    emptyCard extraAttrs =
      Html.span
        (cardAttrs ++ extraAttrs)
        [ Html.text nbsp ]
    viewFreeCell i fc =
      let loc = Model.FreeCell i in
      case fc of
        Nothing -> emptyCard (targetIfOne loc)
        Just c ->
          Html.span
            (cardAttrs ++ sourceAttrs (loc, 1))
            (cardContents c)
    cardsFromSource =
      case drag of
        Nothing -> []
        Just { held } -> Model.cardsFromSource game held
    ghostCards loc =
      let
        ghostCard c =
          Html.span
            (cardAttrs ++ [ Attributes.class "ghost" ])
            (cardContents c)
      in
      case drag of
        Nothing -> []
        Just { held, over } ->
          let (srcLoc, _) = held in
          if srcLoc /= loc && over == Just loc
          then List.map ghostCard cardsFromSource
          else []
    viewCascade i cascade =
      let
        loc = Model.Cascade i
        moveable = Model.initialSequenceLength cascade
        cascadeCard j c =
          let
            attrs =
              [ cardAttrs
              , if j < moveable
                then sourceAttrs (loc, j + 1)
                else []
              ] |> List.concat
          in
          Html.span attrs (cardContents c)
        cascadeWithGhosts = ghostCards loc ++ List.indexedMap cascadeCard cascade
        cascadeOrSlot =
          if List.isEmpty cascadeWithGhosts
          then [ emptyCard [] ]
          else cascadeWithGhosts
      in
      Html.div
        (Attributes.class "cascade" :: targetAttrs loc)
        (List.reverse cascadeOrSlot)
  in
  { title = "FreeCell"
  , body =
      [ Html.div
          [ Attributes.class "foundations" ]
          (Array.toList foundations |> List.indexedMap viewFoundation)
      , Html.div
          [ Attributes.class "freeCells" ]
          (Array.toList freeCells |> List.indexedMap viewFreeCell)
      , Html.div
          [ Attributes.class "cascades" ]
          (List.indexedMap viewCascade (Array.toList cascades))
      , Html.ul
          [ Attributes.class "errors" ]
          (List.map (Html.li [] << List.singleton << Html.text) errors)
      ]
  }


main = Browser.document
  { init = Model.init
  , view = view
  , update = Model.update
  , subscriptions = \model -> Sub.none
  }
