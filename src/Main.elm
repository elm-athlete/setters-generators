module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events

import String.Extra

type alias Model =
  { fields : List String
  , indentSpace : Int
  , newlineBetweenFunctions : Int
  }

type Msg = TextAreaInput String

oneNewline : List String -> String
oneNewline = String.Extra.newlineJoin 1

generateSettersHelp : Int -> Int -> String -> String
generateSettersHelp newlineBetweenFunctions indentSpace fieldName =
  [ setterGeneration indentSpace fieldName
  , setterInGeneration indentSpace fieldName
  ]
    |> String.Extra.newlineJoin newlineBetweenFunctions

setterInGeneration : Int -> String -> String
setterInGeneration indentSpace fieldName =
  let capitalizedFieldName = String.Extra.capitalize fieldName in
  [ "set" ++ capitalizedFieldName ++ "In : { b | " ++ fieldName ++ " : a } -> a -> { b | " ++ fieldName ++ " : a }"
  , "set" ++ capitalizedFieldName ++ "In o v ="
  , "{ o | " ++ fieldName ++ " = v }" |> String.Extra.indent indentSpace
  ]
    |> oneNewline

setterGeneration : Int -> String -> String
setterGeneration indentSpace fieldName =
  let capitalizedFieldName = String.Extra.capitalize fieldName in
  [ "set" ++ capitalizedFieldName ++ " : a -> { b | " ++ fieldName ++ " : a } -> { b | " ++ fieldName ++ ": a }"
  , "set" ++ capitalizedFieldName ++ " v o ="
  , "{ o | " ++ fieldName ++ " = v }" |> String.Extra.indent indentSpace
  ]
    |> oneNewline

generateSetters : Model -> List (Html Msg)
generateSetters { newlineBetweenFunctions, indentSpace, fields }=
  fields
    |> List.filter ((/=) "")
    |> List.map (Html.text << generateSettersHelp newlineBetweenFunctions indentSpace)

toGen : List String
toGen =
  [ "glasses"
  , "eye"
  ]

main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }

init : () -> (Model, Cmd Msg)
init flags =
  (Model [] 4 2, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ fields } as model) =
  case msg of
    TextAreaInput content ->
      ( { model | fields =
          content
            |> String.split "\n"
        }
      , Cmd.none
      )

view : Model -> Browser.Document Msg
view model =
  Browser.Document "Setter Generator"
    [ Html.h1 []
      [ Html.text "Welcome to Setters Generators!" ]
    , Html.h2 []
      [ Html.text "Try to type some field names into the text area!" ]
    , Html.textarea
      [ Html.Events.onInput TextAreaInput
      , Html.Attributes.value (model.fields |> oneNewline)
      , Html.Attributes.rows 20
      , Html.Attributes.cols 100
      ]
      []
    , if List.length model.fields == 0 then
        Html.h1 [] [ Html.text "You typed nothing… Try to enter some field names above!" ]
      else
        model
          |> generateSetters
          |> List.intersperse
            ('\n'
              |> List.repeat model.newlineBetweenFunctions
              |> String.fromList
              |> Html.text
            )
          |> Html.pre []
    ]
