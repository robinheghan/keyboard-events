module Keyboard.Events exposing (Key(..), keyDecoder, onKeyDown, onKeyPress, onKeyUp)

import Html
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)


type Key
    = Character String
    | Alt
    | AltGraph
    | CapsLock
    | Control
    | Fn
    | FnLock
    | Hyper
    | Meta
    | NumLock
    | ScrollLock
    | Shift
    | Super
    | Symbol
    | SymbolLock
    | Enter
    | Tab
    | Spacebar
    | ArrowDown
    | ArrowLeft
    | ArrowRight
    | ArrowUp
    | End
    | Home
    | PageDown
    | PageUp
    | Backspace
    | Clear
    | Copy
    | CrSel
    | Cut
    | Delete
    | EraseEof
    | ExSel
    | Insert
    | Paste
    | Redo
    | Undo
    | F1
    | F2
    | F3
    | F4
    | F5
    | F6
    | F7
    | F8
    | F9
    | F10
    | F11
    | F12
    | F13
    | F14
    | F15
    | F16
    | F17
    | F18
    | F19
    | F20
    | Again
    | Attn
    | Cancel
    | ContextMenu
    | Escape
    | Execute
    | Find
    | Finish
    | Help
    | Pause
    | Play
    | Props
    | Select
    | ZoomIn
    | ZoomOut
    | AppSwitch
    | Call
    | Camera
    | CameraFocus
    | EndCall
    | GoBack
    | GoHome
    | HeadsetHook
    | LastNumberRedial
    | Notification
    | MannerMode
    | VoiceDial
    | ChannelDown
    | ChannelUp
    | MediaFastForward
    | MediaPause
    | MediaPlay
    | MediaPlayPause
    | MediaRecord
    | MediaRewind
    | MediaStop
    | MediaTrackNext
    | MediaTrackPrevious


keyDecoder : Decoder Key
keyDecoder =
    let
        helper : String -> Decoder Key
        helper code =
            case code of
                _ ->
                    Decode.fail "Not a known key code"
    in
    Decode.field "key" Decode.string
        |> Decode.andThen helper


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


onKeyPress : List ( Key, msg ) -> Html.Attribute msg
onKeyPress decisionMap =
    Events.on "keypress" <| messageSelector decisionMap


onKeyDown : List ( Key, msg ) -> Html.Attribute msg
onKeyDown decisionMap =
    Events.on "keydown" <| messageSelector decisionMap


onKeyUp : List ( Key, msg ) -> Html.Attribute msg
onKeyUp decisionMap =
    Events.on "keyup" <| messageSelector decisionMap
