(* klotski.ml: core functionality of the Klotski game. *)
(* Student name:  Dallas Taylor *)
(* CMS cluster login name: dtaylor **********************  *) 

(* ---------------------------------------------------------------------- 
 * Types.
 * ---------------------------------------------------------------------- *)

type loc = int * int
type dir = Up | Down | Left | Right
type move = char * dir * int

module LocM =
  struct
    type t = loc
    let compare = Stdlib.compare
  end

module LocSet : Set.S with type elt = loc = Set.Make(LocM)

(* Sets of LocSets.  Used locally only. *)

module LocSetM =
  struct
    type t = LocSet.t
    let compare = LocSet.compare
  end

module LocSetSet = Set.Make(LocSetM)

module CharM =
  struct
    type t = char
    let compare = Stdlib.compare
  end

module CharMap : Map.S with type key = char = Map.Make(CharM)

type piece = LocSet.t
type t = { pieces : piece CharMap.t ; unoccupied : LocSet.t }

(* ---------------------------------------------------------------------- 
 * Functions.
 * ---------------------------------------------------------------------- *)

(* Create a board from a string. *)
let read s = 
  let rec iter p u r c =
    match () with
      | _ when r = 5 -> { pieces = p; unoccupied = u }
      | _ when c = 4 -> iter p u (r + 1) 0 
      | _ -> 
        let i = r * 4 + c in
        let ch = s.[i] in
          if ch = '.'  (* unoccupied location; add to unoccupied set *)
            then iter p (LocSet.add (r, c) u) r (c + 1)
            else  (* occupied; add to appropriate piece set *)
              try
                let cs  = CharMap.find ch p in     (* old piece set *)
                let cs' = LocSet.add (r, c) cs in  (* add new location *)
                let p'  = CharMap.add ch cs' p in  (* store back into map *)
                  iter p' u r (c + 1)
              with
                Not_found ->  (* new piece; create a new piece set *)
                  let cs = LocSet.singleton (r, c) in
                  let p' = CharMap.add ch cs p in
                    iter p' u r (c + 1)
  in
    if String.length s <> 20
      then failwith "read: invalid input string length"
      else iter CharMap.empty LocSet.empty 0 0

(* Convert the board to a string representation suitable for printing. *)
let show b = 
  let string_of_char_list = function
    | [a;b;c;d] -> Printf.sprintf "%c%c%c%c" a b c d
    | _ -> failwith "invalid char list"
  in
  let char_at board loc =
    let rec iter = function
      | [] -> raise Not_found
      | (c, locs) :: t -> 
        if LocSet.mem loc locs then c else iter t
    in
    if LocSet.mem loc board.unoccupied
      then '.'
      else iter (CharMap.bindings board.pieces)
  in
  (String.concat "\n"
     (List.map (fun r ->
        string_of_char_list
          (List.map (char_at b) 
            (List.map (fun c -> (r, c)) [0; 1; 2; 3])))
        [0; 1; 2; 3; 4])) ^ "\n"

(* Input - (t) board
    Output - (bool) if the board is solved (has a 2x2 block in the bottom rows
    and middle columns) *)
let is_solved b = 
    let r1 = 3 and r2 = 4 and c1 = 1 and c2 = 2 in
    let solvedrc = LocSet.of_list [(r1, c1); (r1, c2); (r2, c1); (r2,c2)] in
    CharMap.exists (fun c locs -> LocSet.equal solvedrc locs) b.pieces 

(* Input - (t t) boards 1 and 2 to compare
    Output - (int) 0 if b1 = b2, 1 if b1 > b2 and -1 if b1 < b2 *)
let compare b1 b2 = 
    let b1unocc = b1.unoccupied and b2unocc = b2.unoccupied and
    b1locs = CharMap.bindings b1.pieces and b2locs = CharMap.bindings b2.pieces
    in
    let unocc = LocSet.compare b1unocc b2unocc in
    if unocc <> 0 then unocc else
    let comp_o = 
        let rec co_helper all = function
        | [] -> all
        | (cs, locs)::t ->co_helper (LocSetSet.add locs all) t
        in
        LocSetSet.compare (co_helper LocSetSet.empty b1locs) 
        (co_helper LocSetSet.empty b2locs)
    in comp_o

(* Input - (char t) piece label to remove from board
    Output - (t) board with piece label and location removed from pieces map
    and added to unoccupied set *)
let remove c ({ pieces = p; unoccupied = u } as b) = 
    if (CharMap.mem c p) then
        let r_locc = CharMap.find c p in
        let r_pieces = CharMap.remove c p in
        let a_unocc = LocSet.union u r_locc in
            {pieces = r_pieces; unoccupied = a_unocc}
    else b

(* Input - (char * LocSet t) piece label and locc to add to board
    Output - ([< None | Some t]) None if the piece cannot be added or 
    Some (board) with added piece *)
let add (c, p) { pieces = ps; unoccupied = u } = 
    if (CharMap.mem c ps) then None else
        if LocSet.subset p u then
            let r_unocc = LocSet.diff u p in
            let a_pieces = CharMap.add c p ps in
            Some {pieces = a_pieces; unoccupied = r_unocc}
        else None

(* Input - (char * dir * int t) piece label, direction, and how far to move the
piece on the board
    Output - ([< None | Some t]) None if the piece cannot be moved or 
    Some (board) with moved piece *)
let make_move (c, d, i) b =
    if (not (CharMap.mem c b.pieces) ) || i < 1 then None else
    (* let cl_helper = function
        | (c, loc) -> LocSet.union LocSet.empty loc in *)
    let curr_loc = CharMap.find c b.pieces in
    let rb = remove c b in
    let move i2 (r, c) = match d with
        | Up -> (r - i2, c) 
        | Down -> (r + i2, c)
        | Left -> (r, c - i2)
        | Right -> (r, c + i2) in
    let check_moves locs = (LocSet.subset locs rb.unoccupied) in
    let rec check_loc all_moves loc = function
        | 0 -> check_moves all_moves
        | n -> check_loc (LocSet.add (move n loc) all_moves) loc (n - 1) in
    let check_all_locs = 
    LocSet.exists (fun loc -> not (check_loc LocSet.empty loc i) ) curr_loc in
    if check_all_locs then None else
        let n_loc = LocSet.map (move i) curr_loc in
        let m_pieces = CharMap.add c n_loc rb.pieces in
        let n_unocc = LocSet.diff rb.unoccupied n_loc in
            Some {pieces = m_pieces; unoccupied = n_unocc}

let next b =
  let max_lr = 3 and max_ud = 4 in
  let try_add (c, d, i) b all_boards = match (make_move (c, d, i) b) with
    | None -> all_boards
    | Some b -> b::all_boards in
  let rec check_ud all_boards c = function
    | 0 -> all_boards
    | n -> let new_boards1 = try_add (c, Up, n) b all_boards in
           let new_boards2 = try_add (c, Down, n) b new_boards1 in
           check_ud new_boards2 c (n - 1) in
  let rec check_lr all_boards c = function
    | 0 -> check_ud all_boards c max_ud
    | n -> let new_boards1 = try_add (c, Left, n) b all_boards in
           let new_boards2 = try_add (c, Right, n) b new_boards1 in
           check_lr new_boards2 c (n - 1) in
  let rec check_all_colors all_boards = function
    | [] -> all_boards
    | c::t -> check_all_colors (check_lr all_boards c max_lr) t in
  let rec ac_helper all = function
        | [] -> all
        | (cs, locs)::t -> ac_helper (cs::all) t
        in
  check_all_colors [] (ac_helper [] (CharMap.bindings b.pieces) )


  

(* Function to interactively test the "next" function. 
 * Useful for debugging. *)
let test_next b =
  let bs = next b in
    begin
      print_string (show b ^ "\n");
      List.iter 
        (fun b -> print_string ("----\n\n" ^ show b ^ "\n"))
        bs
    end

