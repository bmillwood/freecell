module Drag exposing
  ( Model
  , held, over, heldOver, HeldOver
  , init
  , Msg(..), update
  , setTouchConfig
  , sourceAttributes, targetAttributes
  )

import Browser.Dom
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode
import Maybe
import Task

type alias HeldOver source target =
  Maybe
    { held : source
    , over : Maybe target
    }

type alias Touch = { pageX : Float, pageY : Float }

type alias TouchConfig target = { getTarget : Touch -> Maybe target }

type alias Model source target =
  { heldOver : HeldOver source target
  , touchConfig : Maybe (TouchConfig target)
  }

held : Model s t -> Maybe s
held model = Maybe.map .held model.heldOver

over : Model s t -> Maybe t
over model = Maybe.andThen .over model.heldOver

heldOver : Model s t -> HeldOver s t
heldOver = .heldOver

init : Model source target
init = { heldOver = Nothing, touchConfig = Nothing }

type Msg source target
  = Start source
  | Hover (Maybe target)
  | Drop (Maybe target)
  | SetTouchConfig (Maybe (TouchConfig target))

setTouchConfig : { allTargetIds : List (String, target) } -> Cmd (Msg source target)
setTouchConfig { allTargetIds } =
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
  |> Task.perform (\xs -> SetTouchConfig (Just { getTarget = getTarget xs }))

decodeTouch : Json.Decode.Decoder Touch
decodeTouch =
  Json.Decode.map2 Touch
    (Json.Decode.field "pageX" Json.Decode.float)
    (Json.Decode.field "pageY" Json.Decode.float)

decodeTouchEvent : Json.Decode.Decoder Touch
decodeTouchEvent = Json.Decode.at ["changedTouches", "0"] decodeTouch

sourceAttributes
  : source -> Model source target -> List (Html.Attribute (Msg source target))
sourceAttributes source { touchConfig } =
  [ Attributes.draggable "true"
  , Events.on "dragstart" (Json.Decode.succeed (Start source))
  , Events.on "dragend" (Json.Decode.succeed (Drop Nothing))
  ] ++ case touchConfig of
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
  case (msg, model.heldOver) of
    (SetTouchConfig config, _) -> { model | touchConfig = config }
    (Start source, _) -> { model | heldOver = Just { held = source, over = Nothing } }
    (_, Nothing) -> { model | heldOver = Nothing }
    (Hover target, Just ho) -> { model | heldOver = Just { ho | over = target } }
    (Drop _, _) -> { model | heldOver = Nothing }
