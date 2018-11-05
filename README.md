# Keyboard Events

This package extends [ohanhi/keyboard](https://package.elm-lang.org/packages/ohanhi/keyboard/latest/) with functions that produce Html events that fire messages when a given key is pressed. It works as follows:

```
import Html exposing (Html, input)
import Keyboard exposing (Key(..))
import Keyboard.Events as Keyboard

autocomplete : Html Msg
autocomplete =
    input
        [ Keyboard.onKeyDown [(Escape, CloseAutoComplete) ]]
        []
```

## Licence

This package uses the BSD-3 licence.
