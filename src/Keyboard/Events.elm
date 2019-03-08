module Keyboard.Events exposing (Event(..), on, custom, customPerKey)

{-| Send message when a given key is pressed while a certain Html element has focus.

The functions in this namespace uses the keyboard event api, which is supported
in most browsers (including IE11).

This package makes use of a `Keyboard` custom type from the ohanhi/Keyboard package,
so you'll need that as a direct dependency.


# Html Events

@docs Event, on, custom, customPerKey

-}

import Html
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Keyboard exposing (Key)


{-| When should a message be triggered?
`Keydown` triggers when a key on your keyboard is pressed down.
`Keyup` triggers when a key is release.
`Keypress` only triggers if the key produces a character. If you want
to trigger a message when the spacebar is pressed, use `Keydown` instead.
-}
type Event
    = Keydown
    | Keyup
    | Keypress


{-| Listen for the corresponding event, and trigger the message that corresponds with the
given `Key`. Works the same as `Html.Events.on`.
-}
on : Event -> List ( Key, msg ) -> Html.Attribute msg
on event decisionMap =
    Events.on (eventToString event) <|
        messageSelector decisionMap


{-| Works like `on`, but allows you to specify if the default action or propogation should be stopped
if a message is triggered.
-}
custom : Event -> { preventDefault : Bool, stopPropagation : Bool } -> List ( Key, msg ) -> Html.Attribute msg
custom event options decisionMap =
    let
        alteredDecisionMap =
            List.map
                (\( key, msg ) ->
                    ( key
                    , { message = msg
                      , preventDefault = options.preventDefault
                      , stopPropagation = options.stopPropagation
                      }
                    )
                )
                decisionMap
    in
    Events.custom (eventToString event) <|
        messageSelector alteredDecisionMap


{-| Works like `custom`, but allows you to specify if the default action or propogation should be stopped
if a message is triggered for a specific `Key`.
-}
customPerKey :
    Event
    ->
        List
            ( Key
            , { message : msg
              , preventDefault : Bool
              , stopPropagation : Bool
              }
            )
    -> Html.Attribute msg
customPerKey event decisionMap =
    Events.custom (eventToString event) <|
        messageSelector decisionMap


eventToString : Event -> String
eventToString event =
    case event of
        Keydown ->
            "keydown"

        Keyup ->
            "keyup"

        Keypress ->
            "keypress"


messageSelector : List ( Key, a ) -> Decoder a
messageSelector decisionMap =
    let
        helper : Maybe Key -> Decoder a
        helper maybeKey =
            case maybeKey of
                Nothing ->
                    Decode.fail "No key we're interested in"

                Just key ->
                    decisionMap
                        |> List.filter (\( k, _ ) -> k == key)
                        |> List.head
                        |> Maybe.map Tuple.second
                        |> Maybe.map Decode.succeed
                        |> Maybe.withDefault (Decode.fail "No key we're interested in")
    in
    Keyboard.eventKeyDecoder
        |> Decode.map Keyboard.anyKeyUpper
        |> Decode.andThen helper
