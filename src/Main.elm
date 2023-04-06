module Main exposing (main)

import Browser
import Browser.Events
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Matrix as Mat
import Svg as S
import Svg.Attributes as SA
import Task
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- data types


type CellState
    = Blank
    | Conductor
    | Head
    | Tail


type AppState
    = Working
    | Editing
    | Pause


type alias Model =
    { matrix : Mat.Matrix CellState
    , cursorPosn : ( Int, Int )
    , appState : AppState
    }


type Direction
    = ArrowUp
    | ArrowDown
    | ArrowRight
    | ArrowLeft


type Msg
    = Start
    | Stop
    | EditModeOn
    | EditModeOff
    | MoveCursor Direction
    | SetState CellState
    | Tick Time.Posix
    | Nothing



-- constants


matrixRowLength : Int
matrixRowLength =
    20


matrixColLength : Int
matrixColLength =
    30


cursorInitialPosn : ( Int, Int )
cursorInitialPosn =
    ( matrixRowLength // 2, matrixColLength // 2 )


cellSize : Float
cellSize =
    30



-- init


init : () -> ( Model, Cmd Msg )
init _ =
    ( { matrix = Mat.repeat matrixRowLength matrixColLength Blank
      , cursorPosn = cursorInitialPosn
      , appState = Pause
      }
    , Cmd.none
    )



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        when t newmodel =
            if t then
                ( newmodel, Cmd.none )

            else
                ( model, Cmd.none )
    in
    case msg of
        Start ->
            ( { model | appState = Working }, Cmd.none )

        Stop ->
            when (model.appState == Working) { model | appState = Pause }

        EditModeOn ->
            when (model.appState == Pause) { model | appState = Editing }

        EditModeOff ->
            when (model.appState == Editing) { model | appState = Pause }

        SetState state ->
            when (model.appState == Editing) (setStateToCursorPosn state model)

        MoveCursor direction ->
            when (model.appState == Editing) (moveCursor direction model)

        Tick _ ->
            when (model.appState == Working) (updateMatrix model)

        Nothing ->
            ( model, Cmd.none )


setStateToCursorPosn : CellState -> Model -> Model
setStateToCursorPosn state model =
    let
        ( row, col ) =
            model.cursorPosn
    in
    { model | matrix = Mat.set row col state model.matrix }


moveCursor : Direction -> Model -> Model
moveCursor direction model =
    let
        ( row, col ) =
            model.cursorPosn

        rowmax =
            matrixRowLength - 1

        colmax =
            matrixColLength - 1
    in
    case direction of
        ArrowUp ->
            if row == 0 then
                { model | cursorPosn = ( rowmax, col ) }

            else
                { model | cursorPosn = ( row - 1, col ) }

        ArrowDown ->
            if row == rowmax then
                { model | cursorPosn = ( 0, col ) }

            else
                { model | cursorPosn = ( row + 1, col ) }

        ArrowLeft ->
            if col == 0 then
                { model | cursorPosn = ( row, colmax ) }

            else
                { model | cursorPosn = ( row, col - 1 ) }

        ArrowRight ->
            if col == colmax then
                { model | cursorPosn = ( row, 0 ) }

            else
                { model | cursorPosn = ( row, col + 1 ) }


updateMatrix : Model -> Model
updateMatrix model =
    let
        updateCell row col centerstate =
            case centerstate of
                Blank ->
                    Blank

                Head ->
                    Tail

                Tail ->
                    Conductor

                Conductor ->
                    let
                        headCount =
                            neighborhoodHeadCount model.matrix row col
                    in
                    if headCount == 1 || headCount == 2 then
                        Head

                    else
                        Conductor
    in
    { model | matrix = Mat.indexedMap updateCell model.matrix }


neighborhoodHeadCount : Mat.Matrix CellState -> Int -> Int -> Int
neighborhoodHeadCount matrix row col =
    List.sum
        (List.map
            (\diffrow ->
                List.sum
                    (List.map
                        (\diffcol ->
                            if diffrow == 0 && diffcol == 0 then
                                0

                            else if Mat.get (row + diffrow) (col + diffcol) matrix == Just Head then
                                1

                            else
                                0
                        )
                        [ -1, 0, 1 ]
                    )
            )
            [ -1, 0, 1 ]
        )



-- view


view : Model -> H.Html Msg
view model =
    H.div
        [ onKeyDown ]
        [ viewCommandBar model
        , viewMatrix model
        ]


onKeyDown : H.Attribute Msg
onKeyDown =
    HE.on "keypress" keyDecoder


viewCommandBar : Model -> H.Html Msg
viewCommandBar model =
    H.div
        []
        [ H.button
            [ HE.onClick Start
            , HA.disabled (model.appState == Working)
            ]
            [ H.text "Start" ]
        , H.button
            [ HE.onClick Stop
            , HA.disabled (model.appState /= Working)
            ]
            [ H.text "Stop" ]
        , H.div
            []
            [ H.input
                [ HA.type_ "checkbox"
                , HA.id "chkeditmode"
                , HE.onCheck
                    (\t ->
                        if t then
                            EditModeOn

                        else
                            EditModeOff
                    )
                , HA.disabled (model.appState == Working)
                , HA.checked (model.appState == Editing)
                ]
                []
            , H.label
                [ HA.for "chkeditmode" ]
                [ H.text "Edit mode" ]
            ]
        ]


viewMatrix : Model -> H.Html Msg
viewMatrix model =
    let
        ( row, col ) =
            model.cursorPosn
    in
    S.svg
        [ SA.height (String.fromFloat (toFloat matrixRowLength * cellSize))
        , SA.width (String.fromFloat (toFloat matrixColLength * cellSize))
        ]
        (Mat.toList (Mat.indexedMap viewCell model.matrix)
            ++ (if model.appState == Editing then
                    [ viewCursor row col ]

                else
                    []
               )
        )


viewCell : Int -> Int -> CellState -> S.Svg Msg
viewCell row col state =
    S.rect
        [ SA.fill (fillColor state)
        , SA.height (String.fromFloat cellSize)
        , SA.width (String.fromFloat cellSize)
        , SA.transform (translate row col)
        , SA.stroke "gray"
        , SA.strokeWidth "1"
        ]
        []


viewCursor : Int -> Int -> S.Svg Msg
viewCursor row col =
    let
        nonFillRect width color =
            S.rect
                [ SA.height (String.fromFloat cellSize)
                , SA.width (String.fromFloat cellSize)
                , SA.transform (translate row col)
                , SA.stroke color
                , SA.strokeWidth (String.fromInt width)
                , SA.fillOpacity "0"
                ]
                []
    in
    S.svg
        [ SA.fillOpacity "0" ]
        [ nonFillRect 5 "rgb(0,0,0)"
        , nonFillRect 3 "rgb(255,255,255)"
        ]


fillColor : CellState -> String
fillColor state =
    case state of
        Blank ->
            "rgb(0,0,0)"

        Conductor ->
            "rgb(255,255,128)"

        Head ->
            "rgb(0,90,255)"

        Tail ->
            "rgb(255,75,0)"


translate : Int -> Int -> String
translate row col =
    String.concat
        [ "translate("
        , String.fromFloat (toFloat col * cellSize)
        , ","
        , String.fromFloat (toFloat row * cellSize)
        , ")"
        ]



-- subscription


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 250 Tick
        , Browser.Events.onKeyDown keyDecoder
        ]


keyDecoder : D.Decoder Msg
keyDecoder =
    D.map keyToMsg (D.field "key" D.string)


keyToMsg : String -> Msg
keyToMsg key =
    case key of
        "ArrowUp" ->
            MoveCursor ArrowUp

        "ArrowDown" ->
            MoveCursor ArrowDown

        "ArrowLeft" ->
            MoveCursor ArrowLeft

        "ArrowRight" ->
            MoveCursor ArrowRight

        "1" ->
            SetState Blank

        "2" ->
            SetState Conductor

        "3" ->
            SetState Head

        "4" ->
            SetState Tail

        _ ->
            Nothing
