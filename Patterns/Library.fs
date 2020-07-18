#if !INTERACTIVE
module Patterns
#endif
open System

type Cell = 
| Positive
| Negative
| Unknown

type Pattern = 
| BlackP
| WhiteP
| UnknownP
| Anything
| EndOfCells
| ZeroOrMore of Pattern
| OneOrMore of Pattern
| Exactly of int*Pattern
| FewerThan of int*Pattern
| Sequence of Pattern list
| Either of Pattern*Pattern

let _charToCell (ch:char) =
  match (Char.ToLower ch) with
  |'b' -> Positive
  |'w' -> Negative
  |_ -> Unknown

let _cellToString (cell: Cell) =
  match cell with
  | Positive -> "b"
  | Negative -> "w"
  | Unknown -> "."

let _patternToCell (ptn: Pattern) =
  match ptn with
  | BlackP-> Positive 
  | WhiteP -> Negative
  | UnknownP -> Unknown
  | _ -> failwith "Not implemented"

let toCells (str:string)  = 
  let rec helper (index:int) (out:Cell list) = 
    match index<(String.length str) with 
    | false -> out
    | true -> 
      let cell = _charToCell str.[index]
      helper (index+1) (cell::out)

  let unorderedList = helper 0 []
  List.rev unorderedList

let fromCells (cells: Cell list) =
  let seq = List.map _cellToString cells
  String.concat String.Empty seq

//    doMatch "b" BlackP |> shouldEqual (Some "b")
let _ptnToCellFn (key:Pattern) (cells:Cell list) =
  let value = _patternToCell key
  match cells with 
  | [] -> None
  | h::_ -> 
    match (h=value) with
    | true -> Some value
    | _ -> None

let _takeFirstOccurances (ptn:Pattern) (cells:Cell list) =
  let key = _patternToCell ptn
  let rec helper (_in:Cell list) (result:Cell list) =
    match _in with
    | [] -> result
    | h::t ->
      match (h=key) with
      | false -> result 
      | true -> helper t (key::result)
  let result = helper cells []
  (key, (List.rev result))

let patternMatch (_ptn:Pattern) (_cells:Cell list) : Option<Cell List> =
  let rec helper (seq:Pattern) (cellList:Cell list) (result:Cell list) = 
    match seq with 
    | BlackP | WhiteP | UnknownP  -> 
      let ptnLkupResult = _ptnToCellFn seq cellList
      match ptnLkupResult with
      | Some v -> Some (v::[], cellList.Tail)
      | _ -> None 

    | ZeroOrMore ptn -> 
      let _, result = _takeFirstOccurances ptn cellList
      Some (result, cellList.[result.Length ..])

    | OneOrMore ptn ->
      let _, result = _takeFirstOccurances ptn cellList
      match (result.Length > 0) with
      | false -> None
      | true -> Some (result, cellList.[result.Length ..])

    | Exactly (count, ptn) ->
      let _, result = _takeFirstOccurances ptn cellList
      match (result.Length >= count) with
      | false -> None
      | true -> 
        let subset = (List.take count result) 
        let rest = cellList.[count ..]
        Some (subset, rest)
      
    | FewerThan (count, ptn) ->
      match count>0 with
      | false -> None
      | true ->
        let _, result = _takeFirstOccurances ptn cellList
        let subset = 
          match  (result.Length >= count) with
          | true -> (List.take (count-1) result)
          |  _ -> result
        let rest = cellList.[subset.Length ..]
        Some (subset, rest)
      
    | Sequence ptnList ->
      match ptnList with
        | [] -> 
          let seqResult = (List.rev result)
          let rest = cellList.[seqResult.Length ..]
          Some (seqResult, rest)
        | h::t -> 
          let option = helper h cellList result
          match option with
          | None -> option
          | Some (ptnResult, rest) -> 
            let newResult = (ptnResult@result)
            helper (Sequence t) rest newResult
          
    | Either (a, b) ->
      let _, aResult = _takeFirstOccurances a cellList
      let _, bResult = _takeFirstOccurances b cellList

      let noMatch = (aResult.Length = 0) && (bResult.Length = 0) 
      match noMatch with
      | true -> None
      | _ ->
        let ptnResult = 
          match (aResult.Length > bResult.Length) with
          | true -> aResult
          | false -> bResult
        let rest = cellList.[ptnResult.Length ..]
        Some (ptnResult, rest)

    | Anything ->
      match cellList with
      | [] -> None
      | h::t -> 
        Some (h::[], t)

    | EndOfCells ->
      match cellList with
      | [] -> Some (cellList, cellList)
      | _ -> None
  
  //let ptnMatchResult = 
  match (helper _ptn _cells []) with
  | Some (ptnResult, _) -> Some ptnResult
  | _ -> None

// patternMatch (Exactly (2, UnknownP)) (toCells "xxbwwwb")

let find pattern cells = failwith "Not implemented"
 
let map func pattern cells = failwith "Not implemented"