(* Dallas Taylor *)
(* dtaylor@caltech.edu *)

open Storage

(*
 * Helper functions.
 *)

(* Return `true` if a loc is valid on a board of size
 * `nrows` rows by `ncols` columns. *)
let ok_loc nrows ncols (row, col) =
  row >= 0 && row < nrows && col >= 0 && col < ncols

(* Raise an `Invalid_argument` exception due to a bad location.
 * `name` is the name of the function, and `cause` is the
 * reason for the argument being bad. *)
let bad_loc name cause (row, col) =
  let msg = 
    Printf.sprintf "%s: %s;  row = %d col = %d%!" 
      name cause row col
  in
    invalid_arg msg


(*
 * The board module type and module.  
 * It represents the state of the knight's tour solution.
 *)

module type Board =
  sig
    type loc = Loc.t
    type t

    val make                    : int -> int -> t
    val get_dimensions          : t -> int * int
    val get_last                : t -> loc
    val get_index               : t -> loc -> int option
    val get_reachable           : t -> loc -> int option
    val get_loc_counts_from_loc : t -> loc -> (loc * int) list
    val place                   : t -> loc -> t
    val undo                    : t -> t
    val is_solved               : t -> bool
    val get_placed              : t -> loc list
  end

module Make (S: Storage) : Board =
  struct
    type loc = Loc.t

    type t = 
      {
        nrows      : int;
        ncols      : int;
        size       : int;       (* total # of squares on board *)
        placed     : loc list;  (* locations of all knights placed on board *)
        last_index : int;       (* index of last knight placed *)
        indices    : S.t;
        reachable  : S.t
      }

    (* Helper functions. *)

    let knight_moves = [(2, 1); (1, 2); (-1, 2); (-2, 1); (-2, -1); (-1, -2);
                          (1, -2); (2, -1)]

    let check_bounds board loc = 
      ok_loc board.nrows board.ncols loc

    let init_reachable nrows ncols =
      let new_s = S.make nrows ncols in
      let all_loc = List.init (nrows * ncols) 
        (fun i -> ( (i / ncols) , (i mod ncols) ) ) in
      let try_move (r, c) (r_m, c_m) = match ( (r + r_m), (c + c_m) ) with
        | (br, bc) when not (ok_loc nrows ncols (br, bc) ) -> false
        | (r, c) -> true in
      let rec add_count_each_loc (r, c) count all_moves = 
        match all_moves with 
        | [] -> count
        | curr_move::t -> if try_move (r, c) curr_move 
            then add_count_each_loc (r, c) (count + 1) t
            else add_count_each_loc (r, c) count t in
      let add_count_to_storage s (r, c) = 
        let count = add_count_each_loc (r, c) 0 knight_moves in
          S.set s (r, c) count in
      List.fold_left (add_count_to_storage) new_s all_loc

    (* Interface functions. *)

    let make nrows ncols = 
      {
        nrows      = nrows;
        ncols      = ncols;
        size       = nrows * ncols;
        placed     = [];
        last_index = 0;
        indices    = S.make nrows ncols;
        reachable  = init_reachable nrows ncols
      }

    let get_dimensions board =
      (board.nrows, board.ncols)

    let get_last board =
      match board.placed with
        | [] -> raise Not_found
        | h :: _ -> h

    let get_index board loc = 
      if not (check_bounds board loc) then
        bad_loc "get_index" "location off board" loc
      else
        S.get board.indices loc

    let get_reachable board loc = 
      if not (check_bounds board loc) then
        bad_loc "get_reachable" "location off board" loc
      else
        S.get board.reachable loc

    let get_loc_counts_from_loc board loc = 
      let update_loc (r, c) (r_m, c_m) = (r + r_m, c + c_m) in
      let rec each_move loc_counts all_moves = 
        match all_moves with 
        | [] -> loc_counts
        | (r_m, c_m)::t -> 
          let moved_loc = update_loc loc (r_m, c_m) in
          match (S.get board.reachable moved_loc) with
            | None -> each_move loc_counts t
            | Some c -> each_move ( (moved_loc, c)::loc_counts) t in
      if (check_bounds board loc) then each_move [] knight_moves
      else invalid_arg "location is off the board"
    
    let place board loc = 
      let k_moves (r, c) = 
        List.map (fun (kr, kc) -> (r + kr, c + kc) ) knight_moves in
      match loc with 
        | bloc when not (check_bounds board bloc) -> 
            invalid_arg "location is off the board"
        | bloc when List.mem bloc board.placed -> 
            invalid_arg "knight already occupies location"
        | bloc when (board.last_index > 0) && 
          (not (List.mem bloc (k_moves (get_last board) ) ) ) ->
            invalid_arg "not a valid knight move"
        | gloc -> begin 
        let n_placed = gloc::board.placed in
        let n_last_index = board.last_index + 1 in
        let n_indices = S.set board.indices gloc n_last_index in
        let n_reachable = S.remove board.reachable gloc in 
        let n_reachable2 = List.fold_left 
          (fun reach loc -> 
          match S.get reach loc with
           | None -> reach | Some count -> S.set reach loc (count - 1) ) 
          n_reachable (k_moves gloc) in
          {board with placed = n_placed;
          last_index = n_last_index;
          indices = n_indices;
          reachable = n_reachable2}
        end

    let undo board = 
      let k_moves (r, c) = 
        List.map (fun (kr, kc) -> (r + kr, c + kc) ) knight_moves in
      match board.placed with
        | [] -> board
        | h::t -> begin
          let u_placed = t in
          let u_last_index = board.last_index - 1 in
          let last = get_last board in
          let u_indices = S.remove board.indices last in
          let (u_reachable, u_count) = List.fold_left 
            (fun (reach, u_count) loc -> begin match (S.get reach loc) with
              | None -> (reach, u_count) 
              | Some count ->
                ( (S.set reach loc (count + 1) ), (u_count + 1) ) end ) 
                (board.reachable, 0) (k_moves last) in
          {board with placed = u_placed;
          last_index = u_last_index;
          indices = u_indices;
          reachable = S.set u_reachable last u_count}
          end

    let is_solved board = board.last_index = board.size
    let get_placed board = List.rev board.placed
  end