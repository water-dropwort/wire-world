port module Main exposing (main)

import Browser
import Browser.Events
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Matrix as Mat
import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE
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
    = Empty
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
    | ClearAllState
    | MoveCursor Direction
    | SetCursorPosn ( Int, Int )
    | SetState CellState
    | Tick Time.Posix
    | CsvRequested
    | CsvSelected File
    | CsvLoaded String
    | DownloadCsv
    | Noop



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
    25



-- init


init : () -> ( Model, Cmd Msg )
init _ =
    ( { matrix = initialMatrix
      , cursorPosn = cursorInitialPosn
      , appState = Pause
      }
    , Cmd.none
    )


initialMatrix : Mat.Matrix CellState
initialMatrix =
    Mat.repeat matrixRowLength matrixColLength Empty



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
            if model.appState == Editing then
                ( { model | appState = Working }, preventArrowKey False )

            else
                ( { model | appState = Working }, Cmd.none )

        Stop ->
            when (model.appState == Working) { model | appState = Pause }

        EditModeOn ->
            if model.appState == Pause then
                ( { model | appState = Editing }, preventArrowKey True )

            else
                ( model, Cmd.none )

        EditModeOff ->
            if model.appState == Editing then
                ( { model | appState = Pause }, preventArrowKey False )

            else
                ( model, Cmd.none )

        ClearAllState ->
            ( { model | matrix = initialMatrix }, Cmd.none )

        SetState state ->
            when (model.appState == Editing) (setStateToCursorPosn state model)

        MoveCursor direction ->
            when (model.appState == Editing) (moveCursor direction model)

        SetCursorPosn posn ->
            when (model.appState == Editing) { model | cursorPosn = posn }

        Tick _ ->
            when (model.appState == Working) (updateMatrix model)

        CsvRequested ->
            ( model, Select.file [ "text/csv" ] CsvSelected )

        CsvSelected file ->
            ( model, Task.perform CsvLoaded (File.toString file) )

        CsvLoaded content ->
            case toCellStateMatrix content of
                Just mat ->
                    ( { model | matrix = mat }, Cmd.none )

                Nothing ->
                    ( model, showErrorMessage "Failed to import the csv file." )

        DownloadCsv ->
            ( model, Download.string "wireworld.csv" "text/csv" (matrixToCsv model) )

        Noop ->
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
                Empty ->
                    Empty

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


toCellStateMatrix : String -> Maybe (Mat.Matrix CellState)
toCellStateMatrix content =
    let
        lines =
            String.lines content

        toState str =
            case str of
                "1" ->
                    Just Empty

                "2" ->
                    Just Conductor

                "3" ->
                    Just Head

                "4" ->
                    Just Tail

                _ ->
                    Nothing

        appendState strState line =
            Maybe.map2 (\state line_ -> line_ ++ [ state ]) (toState strState) line

        appendLine strLine flattenedLines =
            Maybe.andThen
                (\flattenedLines_ ->
                    let
                        strCols =
                            String.split "," strLine
                    in
                    if List.length strCols == matrixColLength then
                        Maybe.andThen (\stateCols -> Just (flattenedLines_ ++ stateCols)) <|
                            List.foldl appendState (Just []) strCols

                    else
                        Nothing
                )
                flattenedLines
    in
    if List.length lines == matrixRowLength then
        Maybe.andThen (Mat.fromList matrixRowLength matrixColLength) <|
            List.foldl appendLine (Just []) lines

    else
        Nothing


matrixToCsv : Model -> String
matrixToCsv model =
    let
        stateToString state =
            case state of
                Empty ->
                    "1"

                Conductor ->
                    "2"

                Head ->
                    "3"

                Tail ->
                    "4"

        toCsvCell row col state =
            if col < matrixColLength - 1 then
                stateToString state ++ ","

            else if row < matrixRowLength - 1 then
                stateToString state ++ "\u{000D}\n"

            else
                stateToString state
    in
    Mat.indexedMap toCsvCell model.matrix
        |> Mat.toList
        |> String.concat



-- view


view : Model -> H.Html Msg
view model =
    H.div
        [ onKeyDown
        , HA.class "app"
        ]
        [ H.header
            [ HA.class "app__header" ]
            [ H.div
                [ HA.class "header__title" ]
                [ H.text "Wireworld" ]
            ]
        , H.main_
            [ HA.class "app__main" ]
            [ viewCommandBar model
            , viewEditModeCtrl model
            , viewMatrix model
            , viewStateDescription
            ]
        ]


onKeyDown : H.Attribute Msg
onKeyDown =
    HE.on "keypress" keyDecoder


viewCommandBar : Model -> H.Html Msg
viewCommandBar model =
    H.div
        [ HA.class "cmdbar" ]
        [ viewStartButton model
        , viewStopButton model
        , viewExportButton model
        , viewImportButton model
        , viewClearButton model
        ]


viewButton : (Model -> Bool) -> Msg -> String -> Model -> H.Html Msg
viewButton disabledCond act label model =
    H.button
        [ HA.class "cmdbar__button"
        , HA.disabled (disabledCond model)
        , HE.onClick act
        ]
        [ H.text label ]


viewStartButton : Model -> H.Html Msg
viewStartButton model =
    viewButton (\m -> m.appState == Working) Start "Start" model


viewStopButton : Model -> H.Html Msg
viewStopButton model =
    viewButton (\m -> m.appState /= Working) Stop "Stop" model


viewImportButton : Model -> H.Html Msg
viewImportButton model =
    viewButton (\m -> m.appState == Working) CsvRequested "Import" model


viewClearButton : Model -> H.Html Msg
viewClearButton model =
    viewButton (\m -> m.appState == Working) ClearAllState "Clear" model


viewExportButton : Model -> H.Html Msg
viewExportButton model =
    viewButton (\m -> m.appState == Working) DownloadCsv "Export" model


viewEditModeCtrl : Model -> H.Html Msg
viewEditModeCtrl model =
    H.div
        [ HA.class "edtmd" ]
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
            , HA.class "edtmd__checkbox"
            ]
            []
        , H.label
            [ HA.for "chkeditmode"
            , HA.class "edtmd__cblabel"
            ]
            [ H.text "Edit mode" ]
        , H.div
            [ HA.class "edtmddsc" ]
            [ H.text "Press the key to operate."
            , H.ul
                [ HA.class "edtmddscl" ]
                [ H.li
                    [ HA.class "edtmddscl__item" ]
                    [ H.text "Arrow : Move the cursor." ]
                , H.li
                    [ HA.class "edtmddscl__item" ]
                    [ H.text "1 ~ 4 : Set the state to the cell selected by the cursor." ]
                ]
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
        , SA.class "cellmat"
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
        , SE.onClick (SetCursorPosn ( row, col ))
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
        Empty ->
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


viewStateDescription : H.Html Msg
viewStateDescription =
    let
        cellheader contents =
            H.th
                [ HA.class "stdsct__cell"
                , HA.class "stdsct__cell--header"
                ]
                contents

        cellitem contents =
            H.td
                [ HA.class "stdsct__cell"
                , HA.class "stdsct__cell--item"
                ]
                contents

        descItem state label nextStateDesc =
            H.tr
                []
                [ cellitem
                    [ S.svg
                        [ SA.class "stdsct__icon"
                        , SA.class "stdsct__icon--frame"
                        ]
                        [ S.rect
                            [ SA.class "stdsct__icon"
                            , SA.fill (fillColor state)
                            ]
                            []
                        ]
                    , H.span
                        []
                        [ H.text label ]
                    ]
                , cellitem
                    [ H.span
                        []
                        [ nextStateDesc ]
                    ]
                ]
    in
    H.div
        [ HA.class "stdsc" ]
        [ H.table
            [ HA.class "stdsc__table" ]
            [ H.thead
                []
                [ H.tr
                    []
                    [ cellheader [ H.text "State" ]
                    , cellheader [ H.text "Next State" ]
                    ]
                ]
            , H.tbody
                []
                [ descItem Empty "1:Empty" <|
                    H.text "Empty"
                , descItem Conductor "2:Conductor" <|
                    H.div
                        []
                        [ H.text "Head (if there are 1 or 2 Head cells in the Moore neighborhood.)"
                        , H.br [] []
                        , H.text "Conductor (otherwise)"
                        ]
                , descItem Head "3:Head" <|
                    H.text "Tail"
                , descItem Tail "4:Tail" <|
                    H.text "Conductor"
                ]
            ]
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
            SetState Empty

        "2" ->
            SetState Conductor

        "3" ->
            SetState Head

        "4" ->
            SetState Tail

        "Delete" ->
            SetState Empty

        _ ->
            Noop



-- ports


port showErrorMessage : String -> Cmd msg


port preventArrowKey : Bool -> Cmd msg
