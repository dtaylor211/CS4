(* Dallas Taylor *)
(* dtaylor@caltech.edu *)

open Storage
open Board
open Utils

exception Solution_not_found

module type Searcher =
  sig
    val search : int -> int -> int -> int -> bool -> (Loc.t list) option
  end

module Make (S : Storage) : Searcher =
  struct
    module B = Board.Make(S)
    module P = Print(B)

    (* Helper functions go here. *)

    let search nrows ncols start_row start_col print =
      let new_board = B.make nrows ncols in
      let one_knight = B.place new_board (start_row, start_col) in
      let rec search_helper b = 
        if B.is_solved b then
          begin
          if print then 
            P.print_board b false;
            Some (B.get_placed b) 
          end
        else 
        let start = B.get_last b in
        match B.get_loc_counts_from_loc b start with
          | [] -> None
          | (loc1, co1)::t -> let (min_reach_locs, _) = 
            List.fold_left (fun (min_locs, min) (loc, co) -> 
                  if co < min then ([loc], co) else
                   if co = min then (loc::min_locs, min) else (min_locs, min) ) 
                   ([loc1], co1) t in 
            let r_idx = Random.int (List.length min_reach_locs) in
            let r_loc = List.nth min_reach_locs r_idx in
            search_helper (B.place b r_loc) in
      search_helper one_knight

  end

