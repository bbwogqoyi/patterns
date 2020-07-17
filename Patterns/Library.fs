﻿#if !INTERACTIVE
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

let _takeFirstOccurances (key:Cell) (cells:Cell list) =
  let rec helper (_in:Cell list) (result:Cell list) =
    match _in with
    | [] -> result
    | h::t ->
      match (h=key) with
      | false -> result 
      | true -> helper t (key::result)

  let result = helper cells []
  List.rev result

let patternMatch (seq:Pattern) (cells:Cell list) : Option<Cell List> = // failwith "Not implemented"
  //let rec helper (seq:Pattern) (cells:Cell list) (result:Cell list) = 
  match seq with 
  | BlackP | WhiteP | UnknownP  -> 
    let ptnLkupResult = _ptnToCellFn seq cells
    match ptnLkupResult with
    | Some v -> Some (v::[])
    | _ -> None 

  | ZeroOrMore ptn -> 
    let key = _patternToCell ptn
    Some (_takeFirstOccurances key cells)

  | OneOrMore ptn -> // failwith "Not implemented"
    let key = _patternToCell ptn
    let result = _takeFirstOccurances key cells
    match (result.Length > 0) with
    | false -> None
    | true -> Some result

  | Exactly (count, ptn) -> //failwith "Not implemented"
    let key = _patternToCell ptn
    let result = _takeFirstOccurances key cells
    match (result.Length >= count) with
    | true -> Some (List.take count result)
    | _ -> None

  | FewerThan (count, ptn) -> // failwith "Not implemented"
    let key = _patternToCell ptn
    let result = _takeFirstOccurances key cells
    match (count>0), (result.Length >= count) with
    | true, true -> Some (List.take (count-1) result)
    | true, _ -> Some result
    | _ -> None

  | Sequence ptnList -> failwith "Not implemented"
  | Either (a, b) -> failwith "Not implemented"
  | Anything | EndOfCells -> failwith "Not implemented"

// patternMatch (Exactly (2, UnknownP)) (toCells "xxbwwwb")

let find pattern cells = failwith "Not implemented"
 
let map func pattern cells = failwith "Not implemented"