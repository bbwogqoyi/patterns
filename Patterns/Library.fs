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
      | Some v -> Some (result@[v], cellList.Tail)
      | _ -> None 

    | ZeroOrMore ptn -> 
      match cellList.Length>0 with
      | false -> Some (result, cellList)
      | true ->
        let nested = helper ptn cellList result
        match nested with
        | None -> Some (result, cellList)
        | Some (ptnCells, remCells) -> 
          helper seq remCells ptnCells

    | OneOrMore ptn ->
      match cellList.Length > 0 with
      | true ->
        let nested = helper (ZeroOrMore ptn) cellList result
        match nested with
        | Some (ptnCells, remCells) -> 
            match (ptnCells.Length > 0 ) with
            | false -> None
            | true -> Some (ptnCells, remCells)
        | _ -> None
      | false -> None

    | Exactly (count, ptn) ->
      match (count>0) with
      | true ->
        match (helper ptn cellList result) with
        | Some (ptnCells, remCells) -> 
          helper (Exactly (count-1, ptn)) remCells ptnCells
        | _ -> None
      | false -> Some (result, cellList)
      
    | FewerThan (count, ptn) ->
      match count>0 with
      | false -> None
      | true ->
        let nested = helper (ZeroOrMore ptn) cellList List.empty
        match nested with
        | Some (ptnCells, remCells) -> 
          match (ptnCells.Length >= count) with
          | false -> Some (result@ptnCells, remCells)
          | true -> 
            let subset = List.take (count-1) ptnCells
            let rest = cellList.[subset.Length ..]
            Some (result@subset, rest)
        | _ -> failwith "FewerThan only returns 'None' when (count < 0)"
    
    | Either (a, b) ->
      let aTuple = helper a cellList result 
      let bTuple = helper b cellList result
      match (aTuple, bTuple) with
      | Some (aResult, _), Some (bResult, _) ->
        match aResult.Length >= bResult.Length with
        | true -> aTuple
        | false -> bTuple
      | Some x, None | None, Some x ->
        Some x
      | _ -> None

    | Sequence ptnList ->
      match ptnList with
        | [] -> Some (result, cellList)
        | h::t -> 
          let option = helper h cellList result
          match option with
          | None -> option
          | Some (ptnResult, rest) -> 
            helper (Sequence t) rest ptnResult

    | Anything ->
      match cellList with
      | [] -> None
      | h::t -> 
        Some (result@[h], t)

    | EndOfCells ->
      match cellList with
      | _::_ -> None
      | [] -> Some (result, cellList)
  match (helper _ptn _cells []) with
  | Some (ptnResult, _) -> Some ptnResult
  | _ -> None

let find ptn cells = 
  let rec helper (cList:Cell List) (offset:int) =
    let result = patternMatch ptn cList
    match result with
    | None ->
      match cList.Length = 0 with
      | true -> None
      | false ->
        helper cList.Tail (offset+1)
    | Some x -> Some (x, offset) 
  helper cells 0
  
let map (func:(Cell List->Cell List)) (ptn:Pattern) (cells:Cell List) = 
  let rec helper (cList:Cell List) (rList:Cell List) =
    match cList.IsEmpty with
    | true -> rList
    | false ->
      match (find ptn cList) with
      | None -> rList@cList
      | Some (x, offset) ->
        let unmatched, transformed = cList.[..(offset-1)], func x
        let nResult = rList@unmatched@transformed
        let startIndex = 
          match offset + x.Length with
          | 0 -> offset+1
          | v -> v
        helper cList.[startIndex..] nResult
  helper cells List.empty