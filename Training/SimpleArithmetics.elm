module Tests.SimpleArithmetics exposing(
    Msg,
    Model,
    initialModel,
    update,
    config,
    view,
    Permission
    )

import Common exposing(..)
import Html exposing (..)

type Msg = SignClick

type alias Model = InitialModel { } Permission

type Permission = 
    AllowNegativeNumbers | 
    AllowDivision 
  
initialModel : Model
initialModel = { testType = TestSimpleArithmetics, permissions = [
    (AllowNegativeNumbers, Nothing, Year4) ] }

update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
    SignClick -> model ! []

config : TestConfig
config = { name = "Арифметика", sectionType = ArithmeticsSection, testType = TestSimpleArithmetics }


view : Model -> Html Msg
view model =
    div [] [ text "Arithmetics page"]

