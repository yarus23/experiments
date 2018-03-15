module Common exposing(..)

type SectionType = 
    ArithmeticsSection  |
    LanguageSection  |
    OtherSection

type TestType =
    TestEquality |
    TestSimpleArithmetics

type alias TestConfig = {
    sectionType: SectionType,
    name: String,
    testType: TestType
}

type YearOfEducation =
    Year1 |
    Year2 |
    Year3 |
    Year4 |
    Year5 |
    YearMax


type TestMode = Train | Test

type alias Session = {
    year: YearOfEducation,
    mode: TestMode
}

type alias PermissionTuple a = ( a, Maybe Bool, YearOfEducation )
type alias PermissionList a = List (PermissionTuple a)

type alias InitialModel a b = {
    a|testType: TestType,
    permissions: List (PermissionTuple b)
}

getPermission : a -> List (PermissionTuple a) -> ( Maybe Bool, YearOfEducation )
getPermission p list =
    let
        f : PermissionTuple a -> Bool
        f (a,b,c) = a == p
        head = List.filter f list
        |> List.head
    in
        case head of
        Just (a,b,c) -> (b, c)
        Nothing -> Debug.crash "No such permission"

yearToInt : YearOfEducation -> Int
yearToInt y =
    case y of
    Year1 -> 0
    Year2 -> 1
    Year3 -> 2
    Year4 -> 3
    Year5 -> 4
    YearMax -> 11

yearLessThan : YearOfEducation -> YearOfEducation -> Bool
yearLessThan a limitation  =
    (yearToInt a) <= (yearToInt limitation)

checkPermission : PermissionList a -> YearOfEducation -> a -> Bool
checkPermission list year p =
    let
        ( maybebool, y ) = getPermission p list
    in
        case maybebool of
        Just e -> e
        Nothing -> yearLessThan y year