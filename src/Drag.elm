module Drag exposing (..)

import Browser.Dom
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode
import Task

type alias Model source target =
  Maybe
    { held : source
    , over : Maybe target
    }

init : Model source target
init = Nothing

type Msg source target
  = Start source
  | Hover (Maybe target)
  | Drop (Maybe target)

type alias Touch = { pageX : Float, pageY : Float }

type alias TouchConfig target = { getTarget : Touch -> Maybe target }

buildTouchConfig : { allTargetIds : List (String, target) } -> Cmd (TouchConfig target)
buildTouchConfig { allTargetIds } =
  let
    tryTask t = Task.map Just t |> Task.onError (\_ -> Task.succeed Nothing)
    add target maybeElt acc =
      case maybeElt of
        Nothing -> acc
        Just elt -> (elt.element, target) :: acc
    getTarget targets touch =
      case targets of
        [] -> Nothing
        ({ x, y, width, height }, target) :: rest ->
          if touch.pageX >= x && touch.pageX <= x + width
              && touch.pageY >= y && touch.pageY <= y + height
          then Just target
          else getTarget rest touch
  in
  List.foldl
    (\(id, tg) accT -> Task.map2 (add tg) (tryTask (Browser.Dom.getElement id)) accT)
    (Task.succeed [])
    allTargetIds
  |> Task.perform (\xs -> { getTarget = getTarget xs })

decodeTouch : Json.Decode.Decoder Touch
decodeTouch =
  Json.Decode.map2 Touch
    (Json.Decode.field "pageX" Json.Decode.float)
    (Json.Decode.field "pageY" Json.Decode.float)

decodeTouchEvent : Json.Decode.Decoder Touch
decodeTouchEvent = Json.Decode.at ["changedTouches", "0"] decodeTouch

sourceAttributes
  : source -> Maybe (TouchConfig target) -> List (Html.Attribute (Msg source target))
sourceAttributes source touch =
  [ Attributes.draggable "true"
  , Events.on "dragstart" (Json.Decode.succeed (Start source))
  , Events.on "dragend" (Json.Decode.succeed (Drop Nothing))
  ] ++ case touch of
    Nothing -> []
    Just { getTarget } ->
      [ Events.preventDefaultOn "touchstart" (Json.Decode.succeed (Start source, True))
      , Events.on "touchcancel" (Json.Decode.succeed (Drop Nothing))
      , Events.on "touchmove" (decodeTouchEvent |> Json.Decode.map (Hover << getTarget))
      , Events.on "touchend" (decodeTouchEvent |> Json.Decode.map (Drop << getTarget))
      ]

targetAttributes : target -> List (Html.Attribute (Msg source target))
targetAttributes target =
  [ Events.preventDefaultOn "dragover" (Json.Decode.succeed (Hover (Just target), True))
  , Events.on "dragleave" (Json.Decode.succeed (Hover Nothing))
  , Events.on "drop" (Json.Decode.succeed (Drop (Just target)))
  ]

update : Msg s t -> Model s t -> Model s t
update msg model =
  case (msg, model) of
    (Start source, _) -> Just { held = source, over = Nothing }
    (_, Nothing) -> Nothing
    (Hover target, Just ho) -> Just { ho | over = target }
    (Drop _, _) -> Nothing
