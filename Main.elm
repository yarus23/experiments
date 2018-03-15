
module Main exposing (..)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Common exposing(..)
import Tests.Equality as Equality
import Tests.SimpleArithmetics as SimpleArithmetics


type alias SectionPayload = 
    {
        sType: SectionType,
        name: String,
        ids: String,
        state: SectionStateType
    }

type TestModel =
    EqualityModel Equality.Model |
    SimpleArithmeticsModel SimpleArithmetics.Model


type alias Model =
    {
        sections: List SectionPayload,
        test: TestModel,
        items: List TestModel,
        config: List TestConfig,
        session: Session
    }

type Msg =
    ToggleMenu SectionType |
    TestClick TestType |
    EqualityMsg Equality.Msg |
    SimpleArithmeticsMsg SimpleArithmetics.Msg

init : (Model, Cmd Msg)
init =
    let
        list = [ SectionPayload ArithmeticsSection "Арифметика" "menuArithmetics" collapsedSection, 
                 SectionPayload LanguageSection "Русский язык" "menuLanguage" collapsedSection,
                 SectionPayload OtherSection "Разное" "menuOther" collapsedSection ]
        items = [ EqualityModel Equality.initialModel, SimpleArithmeticsModel SimpleArithmetics.initialModel ]
        cfg = [ Equality.config, SimpleArithmetics.config ]
    in
        
    ( Model list (EqualityModel Equality.initialModel) items cfg { year = Year5, mode = Train }, Cmd.map EqualityMsg Equality.initialCmd )

main : Program Never Model Msg
main = 
    Html.program 
        { init = init 
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


getSection : SectionType -> List SectionPayload -> SectionPayload
getSection t list =
    let
       l = List.filter (\f -> f.sType == t) list
       f = case List.head l of
                Just v -> v
                Nothing ->  Debug.crash "Missing section!"
    in
       f


type SectionState = SectionState String String
type SectionStateType = SectionStateType SectionState SectionState

expandedSection : SectionStateType
expandedSection = SectionStateType (SectionState "true" "") (SectionState "false" "collapse collapsed")

collapsedSection : SectionStateType
collapsedSection = oppositeSection expandedSection

oppositeSection : SectionStateType -> SectionStateType
oppositeSection (SectionStateType a b) =
    SectionStateType b a

sectionStateValue : SectionStateType -> String
sectionStateValue (SectionStateType a b) =
    let
        (SectionState a1 b1) = a
    in
        a1

sectionStateCollapsed : SectionStateType -> String
sectionStateCollapsed (SectionStateType a b) =
    let
        (SectionState a1 b1) = a
    in
        b1


getExpandedAttribute : SectionPayload -> Attribute Msg
getExpandedAttribute section =
        section.state 
        |> sectionStateValue
        |> attribute "aria-expanded"

linkAttributes : SectionPayload -> List (Attribute Msg)
linkAttributes section =    
    [
        class ("list-group-item " ++ sectionStateCollapsed section.state),
        getExpandedAttribute section,
        href ("#" ++ section.ids),
        onClick (ToggleMenu section.sType)
    ]

renderItem : TestConfig -> Html Msg
renderItem item = 
    a [ href "#", class "list-group-item", onClick (TestClick item.testType) ] [ text item.name ]

filterTests : SectionType -> List TestConfig -> List TestConfig
filterTests sectionToCheck items =
    List.filter (\v -> v.sectionType == sectionToCheck) items

renderSection : SectionPayload -> List TestConfig -> Html Msg
renderSection section config =
    div [] [
        a (linkAttributes section)
            [
                span [ class "hidden-sm-down" ] [ section.name |> text ]
            ],
            div [ class <| sectionStateCollapsed section.state, section.ids |> id ]
                -- здесь рендерим актуальные тренажеры
                (filterTests section.sType config 
                |> List.map renderItem)
    ]

renderHome : Html Msg
renderHome = div [] [ text "Home"]

renderTest : TestModel -> Html Msg
renderTest test =
    case test of
    EqualityModel model -> Equality.view model |> Html.map EqualityMsg
    SimpleArithmeticsModel model -> SimpleArithmetics.view model |> Html.map SimpleArithmeticsMsg

sideBar : Model -> Html Msg
sideBar ({ sections, items, config } as model) = 
    div [ class "row" ]
    [ div [ class "col-md-3 col-xs-1 pl-0 pr-0", id "sidebar" ]
      [
        div [ class "list-group panel" ]
            (List.map (\v-> renderSection v config) sections)
      ]
    , div [ class "col-md-9 col-xs-11 pl-3 pt-2" ] [

        a [ href "#sidebar" ] [ i [ class "fa fa-navicon fa-lg" ] [ text "asd"]  ]
        , hr [] []
        , div [ class "page-header" ]
        [ h1 [] [ text "Test Name" ] ]
        ,p [ class "lead" ]  [ 
            renderTest model.test
        ]
            
        ]
    ]


view : Model -> Html Msg
view model =
    Grid.containerFluid []         -- Responsive fixed width container
        [ CDN.stylesheet      -- Inlined Bootstrap CSS for use with reactor
          , sideBar model
          , node "link" [ rel "stylesheet", href "sidebar.css" ] []
          , CDN.fontAwesome
        ]

getTest : List TestModel -> TestType -> TestModel 
getTest items testType =
    let
        head = List.head <|
        List.filter (\v-> 
            let
                payloadType = case v of
                        EqualityModel e -> e.testType
                        SimpleArithmeticsModel e -> e.testType
            in
                payloadType == testType            
        ) items
        testModel  = 
        case head of
            Just v -> v
            Nothing -> Debug.crash "Cannot find test"
    in
        testModel

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        callSubUpdate subUpdate testMsg testModel toMsg toModel testType =
            let
                ( newTestModel, newTestCmd ) = subUpdate model.session testMsg testModel                
            in
                { model| test = toModel newTestModel } ! [ Cmd.map toMsg newTestCmd ]
    in
    case ( msg, model.test ) of
        ( ToggleMenu s, _ ) -> 
            let
                newSections = List.map (\v -> if v.sType == s then { v | state = oppositeSection v.state } else v) model.sections
            in
                { model| sections = newSections } ! []

        ( EqualityMsg testMsg, EqualityModel testModel) -> 
                callSubUpdate Equality.update testMsg testModel EqualityMsg EqualityModel TestEquality

        ( SimpleArithmeticsMsg testMsg, SimpleArithmeticsModel testModel) -> 
                callSubUpdate SimpleArithmetics.update testMsg testModel SimpleArithmeticsMsg SimpleArithmeticsModel TestSimpleArithmetics

        ( TestClick s, _ ) ->   
        -- 1. Установить активную страницу
        let             
            newModel = {model| test = getTest model.items s }
        in
            newModel  ! []
        ( _, _ ) ->
            -- Disregard incoming messages that arrived for the wrong page
            model ! []