module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
    exposing
        ( keyCode
        , on
        , onClick
        , onInput
        )
import Json.Decode as Json


main =
    Browser.sandbox
        { init = init, update = update, view = view }



-- model


type alias Model =
    { todo : String
    , todos : List String
    }


init : Model
init =
    { todo = ""
    , todos =
        [ "drink lots of water"
        , "make sure Elm is awesome"
        ]
    }



-- update


type Msg
    = UpdateTodo String
    | KeyDown Int
    | DeleteTodo Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateTodo text ->
            { model | todo = text }

        DeleteTodo id ->
            { model | todos = List.take id model.todos ++ List.drop (id + 1) model.todos }

        KeyDown key ->
            if key == 13 then
                { todos = model.todo :: model.todos
                , todo = ""
                }

            else
                model



-- view


stylesheet =
    let
        tag =
            "link"

        attrs =
            [ attribute "rel" "stylesheet"
            , attribute "property" "stylesheet"
            , attribute "href" "styles.css"
            ]

        children =
            []
    in
    node tag attrs children


todoItem : String -> Int -> Html Msg
todoItem name id =
    li [ class "todo" ]
        [ span [] [ text name ]
        , button [ onClick (DeleteTodo id) ] [ text "x" ]
        ]


todoList : List String -> Html Msg
todoList todos =
    div []
        [ div [] []
        , ul [ class "todo-list" ]
            (List.indexedMap
                (\id text -> todoItem text id)
                todos
            )
        ]


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)


view : Model -> Html Msg
view model =
    div []
        [ stylesheet
        , h1 [ class "title" ] [ text "todos" ]
        , div [ class "app" ]
            [ input
                [ class "todo__input"
                , onKeyDown KeyDown
                , onInput UpdateTodo
                , value model.todo
                ]
                []
            , todoList model.todos
            ]
        ]
