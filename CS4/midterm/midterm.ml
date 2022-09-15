(* name: Dallas Taylor *)
(* email: dtaylor@caltech.edu *)

(*** Midterm Exam ***)

(** Section A **)
(* A.1 *)

(* Input - (string) string to be converted to list
    Output - (char list) list of chars in string *)
let list_of_string string = 
    let rec los_helper i string curr =
        match i with
        | -1 -> curr
        | _ -> los_helper (i - 1) string (string.[i] :: curr)
    in
    los_helper ( (String.length string) - 1) string []


(* A.2 *)

(* Input - ((char list) (char list)) two lists to check for exact matches
    Output - ((char list) (char list)) two lists with exact matches replaced 
    with '_' char
    Recursive Process *)
let remove_exact_matches clist1 clist2 =
    let rec rem_helper_rec = function
        | ([], []) -> ([], [])
        | ([], _::_) | (_::_, []) -> 
            failwith "remove_exact_matches: lists of different lengths"
        | (c1::r1, c2::r2) when c1 = c2 -> 
            let (rest1, rest2) = rem_helper_rec (r1, r2) in 
            ('_'::rest1,'_'::rest2)
        | (c1::r1, c2::r2) -> 
            let (rest1, rest2) = rem_helper_rec (r1, r2) in 
            (c1::rest1,c2::rest2)
    in
    rem_helper_rec (clist1, clist2)

(* Input - ((char list) (char list)) two lists to check for exact matches
    Output - ((char list) (char list)) two lists with exact matches replaced 
    with '_' char
    Iterative Process *)
let remove_exact_matches' clist1 clist2 = 
    let rec rem_helper_iter cl1_cumm cl2_cumm = function
        | ([], []) -> (cl1_cumm, cl2_cumm)
        | ([], _::_) | (_::_, []) ->
            failwith "remove_exact_matches: lists of different lengths"
        | (c1::r1, c2::r2) when c1 = c2 -> 
            rem_helper_iter ('_'::cl1_cumm) ('_'::cl2_cumm) (r1, r2)
        | (c1::r1, c2::r2) ->
            rem_helper_iter (c1::cl1_cumm) (c2::cl2_cumm) (r1, r2)
    in
    rem_helper_iter [] [] ((List.rev clist1), (List.rev clist2))
        
(* It is clear from the above functions that our overall time complexity is
 O(N).  This can be seen since we iterate over each entry in clist1 and clist2
 (length N) simultaneously, and all other calculations are in constant time 
 (including List.rev since it is called last) *)


(* A.3 *)

(* Input - (char (char list)) list to check for char occurrences 
    Output - ([< None | char list]) None if char not in list or char list with
    first occurence of char replaced with '_' *)
let find_and_remove_char c clist = 
    let rec farc_helper cumm c = function
    | [] -> None
    | h::t when h = c-> Some ( (List.rev cumm) @ ('_'::t) )
    | h::t -> farc_helper (h::cumm) c t
    in
    farc_helper [] c clist

(* We can see that the worst case time complexity of this function is if we 
find a match in our sequence at the last index. Thus, we loop through N chars
in clist, and for each iteration we compute _::cumm, (constant time complexity).
For our last iteration, we use List.rev and @, however since these only occur on
our last iteration, we can find that our overall worst case time complexity is 
O(N). *)


(* A.4 *)

(* Input - ((char list) (char list)) target list and guess list to check
    Output - (char list) 'G' if guessed letter is in correct location, 'Y' if
    guessed letter is correct but incorrect location, 'B' if incorrect letter *)
let get_matches target guess =
    let rec gm_helper cumm = function
        | (target, []) -> (List.rev cumm)
        | (target, h::t) when h = '_' -> 
            gm_helper ('G'::cumm) (target, t)
        | (target, h::t) -> match (find_and_remove_char h target) with
                                | None -> gm_helper ('B'::cumm) (target, t)
                                | Some s -> gm_helper ('Y'::cumm) (s, t)
    in
    if (List.length target) <> (List.length guess) 
    then failwith "get_matches: lists of different lengths"
    else gm_helper [] (remove_exact_matches target guess)
    
(* It is clear that our worst case time complexity involves first removing the
exact matches from our target and guess - O(N=5), and then we loop through each
letter in our guess.  In our worst-case, we have no exact matches and thus we
have to call find_and_remove_char (N) times, which we have shown above to 
have worst case time complexity of O(N).  All remaining calculations 
are of constant time complexity, and thus our overall time complexity is
O(N*N) = O(N^2). *)


(* A.5 *)

(* Input - (string (string list)) target and guess list to check for best color
    Output - (char * char list) each letter that has been guessed * the best
    guess it achieved ('G'/'Y'/'B') 
    Helpers:
    - color_comp allows for comparison 'G' > 'Y' > 'B'
    - get_color gives the current best color for a guessed char or [] if it has
      not been guessed
    - update_color uses List.map to check and apply necessary color updates to
      best list
    - update_all_colors recursively iterates through each letter in the guess 
      and its score and updates the best list
    - get_results recursively iterates through each guess and updates all colors
    *)
let get_letter_colors target_s guesses_s =
    let color_comp c1 c2 = match (c1, c2) with 
        | ('G', _) | (_, 'G') -> 'G'
        | ('Y', _) | (_, 'Y') -> 'Y'
        | (c1, c2) -> c1 in
    let get_color g_char best = 
        (List.filter (fun (g, c) -> g = g_char) best) in
    let update_color (g_char, c_char) best = match (get_color g_char best) with
        | [] -> (g_char, c_char)::best
        | _ -> (List.map (fun (g, c) -> 
                if g = g_char 
                then (g, (color_comp c_char c) )
                else (g, c) ) best) in
    let rec update_all_colors result guess_l best = match (result, guess_l) with
        | ([], []) -> best
        | ([], _::_) | (_::_, []) -> 
            failwith "get_letter_colors: lists of different lengths"
        | (h1::t1, h2::t2) -> 
            update_all_colors t1 t2 (update_color (h2, h1) best) in 
    let rec get_results target_s best = function
        | [] -> best
        | hs::t -> get_results target_s 
            (update_all_colors (get_matches (list_of_string target_s)
            (list_of_string hs) ) (list_of_string hs) best) t
    in List.sort (compare) (get_results target_s [] guesses_s)


(* A.6 *)

(* Input - (int) number of bits in our gray codes
    Output - (int list list) list of each gray code in ascending order *)
let rec gray_codes = function
    | i when i < 0 -> invalid_arg "input cannot be less than 0"
    | 1 -> [[0]; [1]]
    | n -> let g1 = gray_codes (n - 1) in
            (List.map (fun x -> 0::x) g1) @
             (List.map (fun x -> 1::x) (List.rev g1) )



(** Section B **)

(* given functions from specs *)
type tree =
  | Leaf
  | Node of int * int * tree * tree   (* depth, value, left/right subtrees *)

(* Depth of an AVL tree. *)
let depth = function
  | Leaf -> 0
  | Node (d, _, _, _) -> d

(* Extract the data value from a node. *)
let data = function
  | Leaf -> failwith "no data"
  | Node (_, v, _, _) -> v

(* Create a new node from two subtrees and a data value.
 * This assumes that the ordering invariant holds i.e.
 * that v is greater than any value in the left subtree
 * and is smaller than any value in the right subtree.  *)
let make_node v l r =
  let d = 1 + max (depth l) (depth r) in  (* compute the correct depth *)
    Node (d, v, l, r)


(* B.1 *)

(* Input - (int tree) int to search tree for
    Output - (bool) true if int is in tree, false otherwise *)
let rec search i = function
    | Leaf -> false
    | Node (_, v, _, _) when v = i -> true
    | Node (_, v, ltree, rtree) -> 
        if v < i then (search i rtree) else (search i ltree)


(* B.2 *)

(* Input - (tree) tree to rotate left
    Output - (tree) tree rotated left, if possible *)
let left_rotate = function
    | Node (d1, v1, ltree1, Node (d2, v2, ltree2, rtree2)) -> 
        make_node v2 (make_node v1 ltree1 ltree2) rtree2
    | _ -> failwith "can't left rotate"

(* Input - (tree) tree to rotate right
    Output - (tree) tree rotated right, if possible *)
let right_rotate = function
    | Node (d1, v1, Node (d2, v2, ltree2, rtree2), rtree1) ->
        make_node v2 ltree2 (make_node v1 rtree2 rtree1)
    | _ -> failwith "can't right rotate"


(* B.3 *)

(* Input - (int tree) int to insert into tree
    Output - (tree) balanced tree with int inserted, if not already in tree *)
let rec insert v t =
  match t with
    | Leaf -> Node (1, v, Leaf, Leaf)  (* base case *)
    | Node (_, v', l, r) ->
      begin
        match () with
          | _ when v < v' ->   (* insert into left subtree *)
            let l' = insert v l in  (* new left subtree *)
              if depth l' - depth r = 2  (* tree is now unbalanced *)
                then
                  if v < data l'
                    then  (* left-left case *)
                      (* new value v is in the left subtree of l';
                         need to do a right rotation of the new tree *)
                      right_rotate (make_node v' l' r)
                    else  (* left-right case *)
                      (* new value v is in the right subtree of l';
                         need to do a left rotation on l'
                         and a right rotation on the resulting tree. *)
                      right_rotate (make_node v' (left_rotate l') r)
                else
                  make_node v' l' r  (* already balanced *)
          | _ when v > v' ->   (* insert into right subtree *)
            let r' = insert v r in
              if depth r' - depth l = 2
                then
                  if v < data r'
                    then (* right-left case *)
                    left_rotate (make_node v' l (right_rotate r') )
                  else (* right-right case *)
                    left_rotate (make_node v' l r')
                else
                  make_node v' l r' (* already balanced *)
          | _ -> t  (* already in tree *)
      end



(* Functions for Testing *)

(* Find the minimum value in an AVL tree. *)
let rec min_avl_tree = function
  | Leaf -> None
  | Node (_, v, l, _) ->
    begin
      match min_avl_tree l with
        | None -> Some v
        | Some l' -> Some l'
    end

(* Find the maximum value in an AVL tree. *)
let rec max_avl_tree = function
  | Leaf -> None
  | Node (_, v, _, r) ->
    begin
      match max_avl_tree r with
        | None -> Some v
        | Some r' -> Some r'
    end

(* Return true if a tree is a valid AVL tree. *)
let rec is_avl_tree = function
  | Leaf -> true
  | Node (d, v, l, r) ->
    let dl = depth l in
    let dr = depth r in
    if is_avl_tree l
      && is_avl_tree r
      && d = 1 + max dl dr
      && abs (dl - dr) < 2
      then  (* check order invariant *)
        match (max_avl_tree l, min_avl_tree r) with
          | (None, None) -> true
          | (None, Some rmin) -> v <= rmin
          | (Some lmax, None) -> v >= lmax
          | (Some lmax, Some rmin) -> v >= lmax && v <= rmin
      else false