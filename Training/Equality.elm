module Tests.Equality exposing(
    Msg, 
    Model,
    initialModel,
    update,
    config,
    view,
    Permission,
    initialCmd  
    )

import Common exposing(..)
import Html exposing (..)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Button as Button
import Html.Attributes exposing (..)
--import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Utilities.Flex as Flex
import Random exposing (..)

type Operator = Division | Addition | Multiplication | Substraction

type Operand = Number Int |
    Expression Int Operator Int 

type alias Operation = {
    left: Operand,
    right: Operand
}

type Sign = LT | GT | EQ

signToString : Sign -> String
signToString s =
        case s of
        LT -> "<"
        GT -> ">"
        EQ -> "="

operandToString : Operand -> String
operandToString op =
    case op of
    Number v -> toString v
    Expression a b c -> 
    let
        operator = case b of
        Division -> " / "
        Addition -> " + "
        Multiplication -> " * "
        Substraction -> " - "
    in
        toString a ++ operator ++ toString c

calcOperand : Operand -> Int
calcOperand op = 
    case op of
    Number v ->  v
    Expression a op b ->
        case op of
            Division -> a // b
            Addition -> a + b
            Multiplication -> a * b
            Substraction -> a - b

checkResult : Sign -> Operation -> Bool
checkResult sign { left, right } =
    let
        l = calcOperand left
        r = calcOperand right
    in
        case sign of
        LT -> l < r
        GT -> l > r
        EQ -> l == r

type Msg = 
    SignClick |
    NewOperation Operation

-- результат при вычитании не отрицательный если не разрешены отрицательные числа
fixSubstraction : Int -> Operator -> Int -> Operand
fixSubstraction l s r =
    if (l-r) < 0 then Expression l s (r - (l-r))
    else Expression l s r

-- целое при делении
-- не деление на ноль
fixDivision  : Int -> Operator -> Int -> Operand
fixDivision l s r =
    if r == 0 && l == 0 then Expression 1 s 1
    else 
    if r == 0 then Expression l s 1
    else
    Expression (l // r * r) s r

fixOperand : (Permission -> Bool) -> Operand -> Operand
fixOperand permitted op =
    case op of
    Number a -> op
    Expression a b c ->
        case b of
            Substraction -> if permitted AllowNegativeNumbers then op else fixSubstraction a b c
            Division -> fixDivision a b c
            _ -> op

randomSign : (Permission -> Bool) -> Generator Operator
randomSign permitted =
    Random.map (\v->
        case v of
        0 -> Substraction
        1 -> if permitted AllowMultiplication then Multiplication else Substraction 
        2 -> Addition
        _ -> if permitted AllowDivision then Division else Addition
    ) (int 0 3)

randomExpression : (Permission -> Bool) -> Generator Operand
randomExpression p =
    Random.map3 (\a sign b-> Expression a sign b) (minMax p) (randomSign p) (minMax p)

minMax : (Permission -> Bool) -> Generator Int
minMax p =
    let
        max = if p AllowResultGreater100 then 500
        else if p AllowResultGreater20 then 50
        else if p AllowResultGreater10 then 10
        else 5
        min = if p AllowNegativeNumbers then -max else 0
    in 
        int min max

randomNumber : (Permission -> Bool) -> Generator Operand
randomNumber p =
    Random.map Number <| minMax p

numberOrExpression : (Permission -> Bool) -> Bool -> Generator Operand
numberOrExpression p b =
    if b then randomExpression p else randomNumber p

randomOperand : (Permission -> Bool) -> Generator Operand
randomOperand p =
    if p AllowExpressions then
        bool |> Random.andThen ( numberOrExpression p )
    else numberOrExpression p False

randomEquality : Generator Sign
randomEquality =
    Random.map (
        \i->
        case i of
        0 -> LT
        1 -> GT
        _ -> EQ ) <| int 0 2

isExpression : Operand -> Bool
isExpression op =
    case op of
    Number i -> False
    Expression a b c -> True

fixEquality : ( Permission -> Bool ) -> Operand -> Operand -> (Operand, Operand)
fixEquality p a b =
    let
        r = fixOperand p b
        l = fixOperand p a
        r2 = if isExpression l then Number (calcOperand l) else r
        l2 = if isExpression r then Number (calcOperand r) else l
        r3 = if not (isExpression l && isExpression r) then l2 else r2  
    in
        ( l2, r3 )

randomOperationSimple : ( Permission -> Bool ) -> Sign -> Generator Operation
randomOperationSimple p s =
    Random.map2 (\a b ->
        -- выровняем статистику появления равенства
        let
            (l, r) = fixEquality p a b
        in 
            { left = l, right = r }) (randomOperand p) (randomOperand p)

randomOperation  : ( Permission -> Bool ) -> Generator Operation
randomOperation p =
    randomEquality |> Random.andThen (randomOperationSimple p)

type alias Model = InitialModel {
    operation: Operation
} Permission

type Permission = 
    AllowNegativeNumbers |
    AllowResultGreater10 |
    AllowResultGreater20 |
    AllowResultGreater100 |
    AllowExpressions |
    AllowDivision |
    AllowMultiplication

initialModel : Model
initialModel = { testType = TestEquality, permissions = [
    (AllowNegativeNumbers, Nothing, Year4),
    (AllowResultGreater10, Nothing, Year2),
    (AllowResultGreater20, Nothing, Year3),
    (AllowResultGreater100, Nothing, Year4),
    (AllowExpressions, Nothing, Year3),
    (AllowDivision, Nothing, Year4)
    ], operation={ right = Number 1, left = Number 1}  }


initialCmd : Cmd Msg
initialCmd =
    let 
        p = initialModel
    in
    Random.generate NewOperation (randomOperation (checkPermission p.permissions YearMax))

update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
    SignClick -> model ! []
    NewOperation e -> { model | operation = e } ! []

config : TestConfig
config = { name = "Знаки сравнения", sectionType = ArithmeticsSection, testType = TestEquality }


view : Model -> Html Msg
view model =
       
    Grid.containerFluid [] [
        Grid.row [ Row.attrs [ class "text-center", Flex.justifyCenter, style [("white-space", "pre")] ] ] [
            Grid.col [ Col.md1 ] [ text <| operandToString model.operation.left ],
            Grid.col [ Col.md2 ] [ text "?"],
            Grid.col [ Col.md1 ] [ text <| operandToString model.operation.right ]
        ],
        Grid.row [ Row.attrs [ class "text-center", Flex.justifyCenter] ] [
            Grid.col [ Col.md4 ] [
                ButtonGroup.buttonGroup [ ButtonGroup.large ] [
                    ButtonGroup.button [ Button.outlinePrimary ] [ text "<" ],
                    ButtonGroup.button [ Button.outlinePrimary ] [ text ">" ],
                    ButtonGroup.button [ Button.outlinePrimary ] [ text "=" ]
                ]
            ]
        ]
    ]