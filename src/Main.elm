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
view model =
  let
    game =
      case model.history of
        [] -> Model.emptyGame
        latest :: _ -> latest
    { foundations, freeCells, cascades } = game
    targetAttrs loc =
      [ if Drag.over model.drag == Just loc
        then [ Attributes.class "hovered" ]
        else []
      , embedDragAttrs (Drag.targetAttributes loc)
      ] |> List.concat
    targetIfOne loc =
      case Drag.held model.drag of
        Nothing -> []
        Just held ->
          let (_, count) = held in
          if count == 1 then targetAttrs loc else []
    cardAttrs = [ Attributes.class "card" ]
    highlightFoundation card =
      if model.highlightFoundation
      then
        let acceptsMe t = t.suit == card.suit && t.rank == card.rank - 1 in
        if Array.isEmpty (Array.filter acceptsMe game.foundations)
        then []
        else [ Attributes.class "canFoundation" ]
      else []
    sourceAttrs ((ourLoc, ourPos) as source) =
      [ if model.highlightSeq && ourPos > 1
        then [ Attributes.class "source" ]
        else []
      , embedDragAttrs (Drag.sourceAttributes source model.drag)
      , case Drag.held model.drag of
          Just (heldLoc, heldCount) ->
            if heldLoc == ourLoc && ourPos <= heldCount
            then [ Attributes.class "ghost" ]
            else []
          _ -> []
      ] |> List.concat
    cardContents c = [ Html.text (rankText c.rank), viewSuit c.suit ]
    viewFoundation i c =
      let
        loc = Model.Foundation i
        attrs =
          [ cardAttrs
          , [ Attributes.id (Model.idForLocation loc) ]
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
      let
        loc = Model.FreeCell i
        idAttr = Attributes.id (Model.idForLocation loc)
      in
      case fc of
        Nothing -> emptyCard (idAttr :: targetIfOne loc)
        Just c ->
          Html.span
            (idAttr :: cardAttrs ++ highlightFoundation c ++ sourceAttrs (loc, 1))
            (cardContents c)
    cardsFromSource =
      case Drag.held model.drag of
        Nothing -> []
        Just held -> Model.cardsFromSource game held
    ghostCards loc =
      let
        ghostCard c =
          Html.span
            (cardAttrs ++ [ Attributes.class "ghost" ])
            (cardContents c)
      in
      case Drag.heldOver model.drag of
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
              , highlightFoundation c
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
        (Attributes.class "cascade"
          :: Attributes.id (Model.idForLocation loc)
          :: targetAttrs loc)
        (List.reverse cascadeOrSlot)
    checkbox id isChecked onCheck text =
      Html.label
        [ Attributes.for id ]
        [ Html.input
            [ Attributes.type_ "checkbox"
            , Attributes.id id
            , Attributes.checked isChecked
            , Events.onCheck onCheck
            ]
            []
        , Html.text text
        ]
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
      , Html.hr [] []
      , Html.div
          []
          [ Html.button
              [ Events.onClick [Model.RequestNewGame] ]
              [ Html.text "restart" ]
          , Html.button
              [ Events.onClick [Model.Undo] ]
              [ Html.text "undo" ]
          ]
      , Html.p
          []
          [ Html.text "highlight:"
          , Html.ul []
              [ Html.li []
                  [ checkbox
                      "highlightSeq"
                      model.highlightSeq
                      (List.singleton << Model.SetHighlightSeq)
                      "sequences"
                  ]
              , Html.li []
                  [ checkbox
                      "highlightFoundation"
                      model.highlightFoundation
                      (List.singleton << Model.SetHighlightFoundation)
                      "next foundation"
                  ]
              ]
          ]
      , Html.ul
          [ Attributes.class "errors" ]
          (List.map (Html.li [] << List.singleton << Html.text) model.errors)
      ]
  }


main = Browser.document
  { init = Model.init
  , view = view
  , update = Model.update
  , subscriptions = \model -> Sub.none
  }
