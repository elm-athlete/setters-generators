module String.Extra exposing (capitalize, indent, newlineJoin)

capitalize : String -> String
capitalize =
  String.toList
    >> List.indexedMap capitalizeFirst
    >> String.fromList

capitalizeFirst : Int -> Char -> Char
capitalizeFirst index char =
  if index == 0 then
    char |> Char.toUpper
  else
    char

indent : Int -> String -> String
indent width content =
  ' '
  |> List.repeat width
  |> String.fromList
  |> appendInFront content

appendInFront : String -> String -> String
appendInFront content front =
  front ++ content

newlineJoin : Int -> List String -> String
newlineJoin height =
  String.join (String.fromList (List.repeat height '\n'))
