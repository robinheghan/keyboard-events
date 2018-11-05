module Keyboard.Events exposing
    ( onKeyDown, onKeyPress, onKeyUp
    , keyDecoder
    )

{-| Functions that send messages based on key events.

The functions in this namespace uses the keyboard event api, which is supported
in most browsers (including IE11).


# Html Events

@docs onKeyDown, onKeyPress, onKeyUp


# Decoder

@docs keyDecoder

-}

import Html
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Keyboard exposing (Key(..))


keyFromString : String -> Decoder Key
keyFromString code =
    case code of
        -- Modifiers
        "Alt" ->
            Decode.succeed Alt

        "AltGraph" ->
            Decode.succeed AltGraph

        "CapsLock" ->
            Decode.succeed CapsLock

        "Control" ->
            Decode.succeed Control

        "Fn" ->
            Decode.succeed Fn

        "FnLock" ->
            Decode.succeed FnLock

        "Hyper" ->
            Decode.succeed Hyper

        "Meta" ->
            Decode.succeed Meta

        "NumLock" ->
            Decode.succeed NumLock

        "ScrollLock" ->
            Decode.succeed ScrollLock

        "Shift" ->
            Decode.succeed Shift

        "Super" ->
            Decode.succeed Super

        -- Firefox
        "OS" ->
            Decode.succeed Super

        "Symbol" ->
            Decode.succeed Symbol

        "SymbolLock" ->
            Decode.succeed SymbolLock

        -- Whitespace
        "Enter" ->
            Decode.succeed Enter

        "Tab" ->
            Decode.succeed Tab

        "Spacebar" ->
            Decode.succeed Spacebar

        " " ->
            Decode.succeed Spacebar

        -- Navigation
        "ArrowDown" ->
            Decode.succeed ArrowDown

        "ArrowLeft" ->
            Decode.succeed ArrowLeft

        "ArrowRight" ->
            Decode.succeed ArrowRight

        "ArrowUp" ->
            Decode.succeed ArrowUp

        "Down" ->
            Decode.succeed ArrowDown

        "Left" ->
            Decode.succeed ArrowLeft

        "Right" ->
            Decode.succeed ArrowRight

        "Up" ->
            Decode.succeed ArrowUp

        "End" ->
            Decode.succeed End

        "Home" ->
            Decode.succeed Home

        "PageDown" ->
            Decode.succeed PageDown

        "PageUp" ->
            Decode.succeed PageUp

        "Backspace" ->
            Decode.succeed Backspace

        "Clear" ->
            Decode.succeed Clear

        "Copy" ->
            Decode.succeed Copy

        "CrSel" ->
            Decode.succeed CrSel

        "Cut" ->
            Decode.succeed Cut

        "Delete" ->
            Decode.succeed Delete

        "EraseEof" ->
            Decode.succeed EraseEof

        "ExSel" ->
            Decode.succeed ExSel

        "Insert" ->
            Decode.succeed Insert

        "Paste" ->
            Decode.succeed Paste

        "Redo" ->
            Decode.succeed Redo

        "Undo" ->
            Decode.succeed Undo

        "F1" ->
            Decode.succeed F1

        "F2" ->
            Decode.succeed F2

        "F3" ->
            Decode.succeed F3

        "F4" ->
            Decode.succeed F4

        "F5" ->
            Decode.succeed F5

        "F6" ->
            Decode.succeed F6

        "F7" ->
            Decode.succeed F7

        "F8" ->
            Decode.succeed F8

        "F9" ->
            Decode.succeed F9

        "F10" ->
            Decode.succeed F10

        "F11" ->
            Decode.succeed F11

        "F12" ->
            Decode.succeed F12

        "F13" ->
            Decode.succeed F13

        "F14" ->
            Decode.succeed F14

        "F15" ->
            Decode.succeed F15

        "F16" ->
            Decode.succeed F16

        "F17" ->
            Decode.succeed F17

        "F18" ->
            Decode.succeed F18

        "F19" ->
            Decode.succeed F19

        "F20" ->
            Decode.succeed F20

        -- UI
        "Again" ->
            Decode.succeed Again

        "Attn" ->
            Decode.succeed Attn

        "Cancel" ->
            Decode.succeed Cancel

        "ContextMenu" ->
            Decode.succeed ContextMenu

        "Escape" ->
            Decode.succeed Escape

        "Execute" ->
            Decode.succeed Execute

        "Find" ->
            Decode.succeed Find

        "Finish" ->
            Decode.succeed Finish

        "Help" ->
            Decode.succeed Help

        "Pause" ->
            Decode.succeed Pause

        "Play" ->
            Decode.succeed Play

        "Props" ->
            Decode.succeed Props

        "Select" ->
            Decode.succeed Select

        "ZoomIn" ->
            Decode.succeed ZoomIn

        "ZoomOut" ->
            Decode.succeed ZoomOut

        -- Phone
        "AppSwitch" ->
            Decode.succeed AppSwitch

        "Call" ->
            Decode.succeed Call

        "Camera" ->
            Decode.succeed Camera

        "CameraFocus" ->
            Decode.succeed CameraFocus

        "EndCall" ->
            Decode.succeed EndCall

        "GoBack" ->
            Decode.succeed GoBack

        "GoHome" ->
            Decode.succeed GoHome

        "HeadsetHook" ->
            Decode.succeed HeadsetHook

        "LastNumberRedial" ->
            Decode.succeed LastNumberRedial

        "Notification" ->
            Decode.succeed Notification

        "MannerMode" ->
            Decode.succeed MannerMode

        "VoiceDial" ->
            Decode.succeed VoiceDial

        -- Media
        "ChannelDown" ->
            Decode.succeed ChannelDown

        "ChannelUp" ->
            Decode.succeed ChannelUp

        "MediaFastForward" ->
            Decode.succeed MediaFastForward

        "MediaPause" ->
            Decode.succeed MediaPause

        "MediaPlay" ->
            Decode.succeed MediaPlay

        "MediaPlayPause" ->
            Decode.succeed MediaPlayPause

        "MediaRecord" ->
            Decode.succeed MediaRecord

        "MediaRewind" ->
            Decode.succeed MediaRewind

        "MediaStop" ->
            Decode.succeed MediaStop

        "MediaTrackNext" ->
            Decode.succeed MediaTrackNext

        "MediaTrackPrevious" ->
            Decode.succeed MediaTrackPrevious

        _ ->
            if String.length code == 1 then
                Decode.succeed <| Character code

            else
                Decode.fail "Unknown key code"


{-| This decoder is supposed to be used with html events. It detects which
key has been pressed and returns it.

    Html.Events.on "keydown" <| Json.map KeyPressed keyDecoder

-}
keyDecoder : Decoder Key
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen keyFromString


messageSelector : List ( Key, msg ) -> Decoder msg
messageSelector decisionMap =
    let
        helper : Key -> Decoder msg
        helper key =
            decisionMap
                |> List.filter (\( k, _ ) -> k == key)
                |> List.head
                |> Maybe.map Tuple.second
                |> Maybe.map Decode.succeed
                |> Maybe.withDefault (Decode.fail "No key we are interested in")
    in
    keyDecoder
        |> Decode.andThen helper


{-| Takes a list of key-message pairs and triggers a message if the
corresponding key is pressed.
-}
onKeyDown : List ( Key, msg ) -> Html.Attribute msg
onKeyDown decisionMap =
    Events.on "keydown" <| messageSelector decisionMap


{-| Takes a list of key-message pairs and triggers a message if the
corresponding key is pressed. This differs from `onKeyDown` in that it
will only trigger a message if the associated key produces a character.

If you want to trigger actions when a control key is pressed (like the Escape key), use `onKeyDown` instead.

-}
onKeyPress : List ( Key, msg ) -> Html.Attribute msg
onKeyPress decisionMap =
    Events.on "keypress" <| messageSelector decisionMap


{-| Takes a list of key-message pairs and triggers a message if the
corresponding key is released.
-}
onKeyUp : List ( Key, msg ) -> Html.Attribute msg
onKeyUp decisionMap =
    Events.on "keyup" <| messageSelector decisionMap
