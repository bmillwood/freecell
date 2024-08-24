module Drag exposing (..)

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode

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

sourceAttributes : source -> List (Html.Attribute (Msg source target))
sourceAttributes source =
  [ Attributes.draggable "true"
  , Events.on "dragstart" (Json.Decode.succeed (Start source))
  , Events.on "dragend" (Json.Decode.succeed (Drop Nothing))
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
