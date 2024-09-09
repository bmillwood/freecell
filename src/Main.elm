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
        (latest, _) :: _ -> latest
        [] -> Model.emptyGame
    { foundations, freeCells, cascades } = game
    andDrop src = (src, Model.dropLocation src)
    targetAttrs loc =
      [ if Drag.over model.drag == Just loc
        then [ Attributes.class "hovered" ]
        else []
      , embedDragAttrs (Drag.targetAttributes loc)
      ] |> List.concat
    cardAttrs = [ Attributes.class "card" ]
    highlightFoundation card =
      if model.highlightFoundation
      then
        let acceptsMe t = t.suit == card.suit && t.rank == card.rank - 1 in
        if Array.isEmpty (Array.filter acceptsMe game.foundations)
        then []
        else [ Attributes.class "canFoundation" ]
      else []
    sourceAttrs src =
      let
        thisIsHeld =
          case (src, Drag.held model.drag) of
            (Model.Cascade i pos, Just (Model.Cascade j c)) ->
              i == j && pos <= c
            (otherSrc, Just otherHeld) ->
              otherSrc == otherHeld
            _ -> False
      in
      [ case src of
          Model.Cascade _ ourPos ->
            if model.highlightSeq && ourPos > 1
            then [ Attributes.class "source" ]
            else []
          _ -> []
      , embedDragAttrs (Drag.sourceAttributes src model.drag)
      , if thisIsHeld then [ Attributes.class "ghost" ] else []
      ] |> List.concat
    cardContents c = [ Html.text (rankText c.rank), viewSuit c.suit ]
    viewFoundation i c =
      let
        attrs =
          [ cardAttrs
          , if c.rank > 0 then sourceAttrs (Model.Foundation i) else []
          ] |> List.concat
      in
      Html.span attrs (cardContents c)
    emptyCard extraAttrs =
      Html.span
        (cardAttrs ++ extraAttrs)
        [ Html.text nbsp ]
    viewFreeCell i fc =
      let
        (src, dst) = andDrop (Model.FreeCell i)
        idAttr = Attributes.id (Model.idForDrop dst)
        emptyTarget =
          case Drag.held model.drag of
            Nothing -> []
            Just (Model.Cascade _ count) ->
              if count == 1 then targetAttrs dst else []
            Just _ -> targetAttrs dst
      in
      case fc of
        Nothing -> emptyCard (idAttr :: emptyTarget)
        Just c ->
          Html.span
            (idAttr :: cardAttrs ++ highlightFoundation c ++ sourceAttrs src)
            (cardContents c)
    cardsFromSource =
      case Drag.held model.drag of
        Nothing -> []
        Just held -> Model.cardsFromSource game held
    ghostCard c =
      Html.span
        (cardAttrs ++ [ Attributes.class "ghost" ])
        (cardContents c)
    viewCascade i cascade =
      let
        dropLoc = Model.ToCascade i
        moveable = Model.initialSequenceLength cascade
        isHeld =
          case Drag.held model.drag of
            Just (Model.Cascade j _) -> i == j
            _ -> False
        ghostCards =
          case Drag.over model.drag of
            Just (Model.ToCascade j) ->
              if not isHeld && i == j
              then List.map ghostCard cardsFromSource
              else []
            _ -> []
        cascadeCard j c =
          let
            attrs =
              [ cardAttrs
              , highlightFoundation c
              , if j < moveable
                then sourceAttrs (Model.Cascade i (j + 1))
                else []
              ] |> List.concat
          in
          Html.span attrs (cardContents c)
        cascadeWithGhosts = ghostCards ++ List.indexedMap cascadeCard cascade
        cascadeOrSlot =
          if List.isEmpty cascadeWithGhosts
          then [ emptyCard [] ]
          else cascadeWithGhosts
      in
      Html.div
        (Attributes.class "cascade"
          :: Attributes.id (Model.idForDrop dropLoc)
          :: targetAttrs dropLoc)
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
      [ let
          attrs =
            [ Attributes.class "foundations"
            , Attributes.id (Model.idForDrop Model.ToFoundation)
            ] ++ targetAttrs Model.ToFoundation
        in
        Html.div attrs
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
              [ Html.text "new game" ]
          , Html.button
              [ Events.onClick [Model.Restart] ]
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
