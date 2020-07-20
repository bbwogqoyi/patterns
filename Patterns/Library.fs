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
  | _ -> failwith "Pattern does not represent any cell case"

let _isPatternMatchingToCell (ptn: Pattern) (cell:Cell) =
  match ptn with
  | BlackP -> Positive = cell
  | WhiteP -> Negative = cell
  | UnknownP -> Unknown = cell
  | Anything -> true
  | _ -> failwith "Pattern does not represent any cell case"


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

let _takeOccurances (ptn:Pattern) (cells:Cell list) =
  let rec helper (_in:Cell list) (result:Cell list) =
    match _in with
    | [] -> (List.rev result), _in 
    | h::t ->
      match (_isPatternMatchingToCell ptn h) with
      | false -> (List.rev result), _in 
      | true -> helper t (h::result)
  helper cells [] 

let patternMatch (_ptn:Pattern) (_cells:Cell list) : Option<Cell List> =
  let check (ptn:Pattern) (cells:Cell list) =
    match cells with 
    | [] -> None
    | h::_ -> 
      match _isPatternMatchingToCell ptn h with
      | true -> Some h
      | _ -> None

  let rec helper (seq:Pattern) (cellList:Cell list) (result:Cell list) = 
    match seq with 
    | BlackP | WhiteP | UnknownP  -> 
      let option = check seq cellList
      match option with
      | Some v -> Some (v::[], cellList.Tail)
      | _ -> None 

    | ZeroOrMore ptn -> 
      let nested = helper ptn cellList result
      match nested with
      | None -> Some (result, cellList)
      | Some (ptnCells, remCells) -> 
        let newResult = result@ptnCells
        helper seq remCells newResult

    | OneOrMore ptn ->
      let nested = helper (ZeroOrMore ptn) cellList result
      match nested with
      | Some (ptnCells, remCells) -> 
          match (ptnCells.Length > 0 ) with
          | false -> None
          | true -> Some (ptnCells, remCells)
      | _ -> None

    | Exactly (count, ptn) ->
      let nested = helper (ZeroOrMore ptn) cellList result
      match nested with
      | Some (ptnCells, _) -> 
        match (ptnCells.Length >= count) with
        | false -> None
        | true -> 
          let subset = (List.take count ptnCells) 
          let rest = cellList.[count ..]
          Some (subset, rest)
      | _ -> None
      
    | FewerThan (count, ptn) ->
      match count>0 with
      | false -> None
      | true ->
        let nested = helper (ZeroOrMore ptn) cellList result
        match nested with
        | Some (ptnCells, remCells) -> 
          match (ptnCells.Length >= count) with
          | false -> Some (ptnCells, remCells)
          | true -> 
            let subset = List.take (count-1) ptnCells
            let rest = cellList.[subset.Length ..]
            Some (subset, rest)
        | _ -> failwith "FewerThan only returns 'None' when (count < 0)"
    
    | Either (a, b) ->
      let aResult, aRest = _takeOccurances a cellList
      let bResult, bRest = _takeOccurances b cellList

      let noMatch = (aResult.Length = 0) && (bResult.Length = 0) 
      match noMatch with
      | true -> None
      | _ ->
        match (aResult.Length > bResult.Length) with
        | true -> Some (aResult, aRest)
        | false -> Some (bResult, bRest)

    | Sequence ptnList ->
      match ptnList with
        | [] -> Some (result, cellList)
        | h::t -> 
          let option = helper h cellList result
          match option with
          | None -> option
          | Some (ptnResult, rest) -> 
            let newResult = (result@ptnResult)
            helper (Sequence t) rest newResult

    | Anything ->
      match cellList with
      | [] -> None
      | h::t -> 
        Some (h::[], t)

    | EndOfCells ->
      match cellList with
      | _::_ -> None
      | [] -> Some (cellList, cellList)
      
   
  match (helper _ptn _cells []) with
  | Some (ptnResult, _) -> Some ptnResult
  | _ -> None

// patternMatch (Exactly (2, UnknownP)) (toCells "xxbwwwb")

let find pattern cells = failwith "Not implemented"
 
let map func pattern cells = failwith "Not implemented"