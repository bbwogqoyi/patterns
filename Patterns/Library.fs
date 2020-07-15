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

let _singleton (ptn:Pattern) (cells:Cell list) : Option<Cell> =
  let find (cell:Cell) = 
    List.tryFind (fun x -> x=cell) cells
  match ptn with 
  | BlackP -> find Positive
  | WhiteP -> find Negative
  | UnknownP -> find Unknown
  | _ -> failwith "Not implemented"

let patternMatch (seq:Pattern) (cells:Cell list) = // failwith "Not implemented"
  match seq with
  | BlackP | WhiteP | UnknownP | Anything | EndOfCells -> _singleton seq cells
  | ZeroOrMore ptn -> failwith "Not implemented"
  | OneOrMore ptn -> failwith "Not implemented"
  | Exactly (count, ptn) -> failwith "Not implemented"
  | FewerThan (count, ptn) -> failwith "Not implemented"
  | Sequence ptnList -> failwith "Not implemented"
  | Either (a, b) -> failwith "Not implemented"

// patternMatch (Exactly (2, UnknownP)) (toCells "xxbwwwb")

let find pattern cells = failwith "Not implemented"
 
let map func pattern cells = failwith "Not implemented"