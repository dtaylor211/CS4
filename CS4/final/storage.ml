(* Dallas Taylor *)
(* dtaylor@caltech.edu *)

module Loc =
  struct
    type t = int * int

    let compare = Stdlib.compare
  end

module type Storage =
  sig
    type t
    type loc = Loc.t

    val make    : int -> int -> t
    val get     : t -> loc -> int option
    val set     : t -> loc -> int -> t
    val has_loc : t -> loc -> bool
    val remove  : t -> loc -> t
  end

(*
 * Imperative implementation.
 * The data representation is an array of arrays of integers.
 * A null location is represented by the number -1 stored at the location.
 *)
module ImpStorage : Storage =
  struct
    type t   = int array array
    type loc = Loc.t

    let make nrows ncols = match (nrows, ncols) with
      | (i, _ ) when i < 1 -> invalid_arg "nrows needs to be > 0"
      | (_, i) when i < 1 -> invalid_arg "ncols needs to be > 0"
      | (r, c) -> Array.make_matrix r c (-1)

    let get data (row, col) = match (row, col) with
      | (br, _) when br < 0 || br >= Array.length data -> None
      | (_, bc) when bc < 0 || bc >= Array.length data.(0) -> None
      | (br, bc) when data.(br).(bc) = (-1) -> None
      | (r, c) -> Some data.(r).(c)

    let set data (row, col) i = 
      if i < 0 then invalid_arg "integer to store must be >= 0" else
        match (row, col) with
          | (br, _) when br < 0 || br >= Array.length data -> invalid_arg "row 
              needs to be within board bounds"
          | (_, bc) when bc < 0 || bc >= Array.length data.(0) -> invalid_arg 
              "column needs to be within board bounds"
          | (r, c) -> begin data.(r).(c) <- i; data end

    let has_loc data (row, col) = match (row, col) with
      | (br, _) when br < 0 || br >= Array.length data -> false
      | (_, bc) when bc < 0 || bc >= Array.length data.(0) -> false
      | (r, c) -> if data.(r).(c) = (-1) then false else true

    let remove data (row, col) = match (row, col) with
      | (br, _) when br < 0 || br >= Array.length data -> data
      | (_, bc) when bc < 0 || bc >= Array.length data.(0) -> data
      | (r, c) -> if data.(r).(c) = (-1) then data 
          else begin data.(r).(c) <- (-1); data end
  end

(*
 * Functional implementation.
 * The data representation is a map between locs and integers.
 * A null location is represented by the absence of the loc in the map.
 *)
module FunStorage : Storage =
  struct
    module LocMap = Map.Make(Loc)

    type t = 
      {
        contents : int LocMap.t;
        nrows    : int;
        ncols    : int
      }

    type loc = Loc.t

    let make nrows ncols = match (nrows, ncols) with
      | (i, _ ) when i < 1 -> invalid_arg "nrows needs to be > 0"
      | (_, i) when i < 1 -> invalid_arg "ncols needs to be > 0"
      | (r, c) -> {contents = LocMap.empty; nrows = nrows; ncols = ncols}
    
    let get data (row, col) = match (row, col) with
      | (br, _) when br < 0 || br >= data.nrows -> None
      | (_, bc) when bc < 0 || bc >= data.ncols -> None
      | (br, bc) when not (LocMap.mem (br, bc) data.contents) -> None
      | (r, c) -> Some (LocMap.find (r, c) data.contents)

    let set data (row, col) i = 
      if i < 0 then invalid_arg "integer to store must be >= 0" else
        match (row, col) with
          | (br, _) when br < 0 || br >= data.nrows -> invalid_arg "row 
              needs to be within board bounds"
          | (_, bc) when bc < 0 || bc >= data.ncols -> invalid_arg 
              "column needs to be within board bounds"
          | (r, c) -> {data with contents = 
            (LocMap.update (r, c) (fun _ -> Some i) data.contents) }

    let has_loc data (row, col) = match (row, col) with
      | (br, _) when br < 0 || br >= data.nrows -> false
      | (_, bc) when bc < 0 || bc >= data.ncols -> false
      | (r, c) -> if LocMap.mem (r, c) data.contents then true else false

    let remove data (row, col) = match (row, col) with
      | (br, _) when br < 0 || br >= data.nrows -> data
      | (_, bc) when bc < 0 || bc >= data.ncols -> data
      | (r, c) -> if LocMap.mem (r, c) data.contents 
          then {data with contents = (LocMap.remove (r, c) data.contents) }
          else data
  end

