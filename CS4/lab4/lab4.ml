(*** Lab 4 - Dallas Taylor ***)

(** Section A **)
(* A.1 *)

(* Type definitions for point and segment *)
type point = { x : float; y : float }
type segment = { startp : point; endp : point }

(* Returns the midpoint of the input segment -- Input type segment -- Output type point *)
let midpoint_segment { startp; endp } = 
    let x = (startp.x +. endp.x) /. 2. in
    let y = (startp.y +. endp.y) /. 2. in
        { x; y }

(* Returns the length of the input segment -- Input type segment -- Output type float *)
let segment_length { startp; endp } = 
    let square m = m *. m in
    let dif c1 c2 = c2 -. c1 in
    sqrt ( (square (dif startp.x endp.x) ) +. (square (dif startp.y endp.y) ) )

(* Prints the current point to the terminal as a tuple -- Input type point -- Output type unit *)
let print_point { x; y } = 
    Printf.printf "(%g, %g)\n" x y

(* Creates a point type from float x and y -- Input type float float -- Output type point *)
let make_point x y = 
    let x = x in
    let y = y in
    { x; y }

(* Returns the 2-tuple of the x and y coordinates of the input point -- Input type point -- 
Output type float * float *)
let get_coords { x; y } = 
    (x, y)

(* Returns the segment created by our two input points -- Input type point point -- Output type segment *)
let make_segment startp endp = 
    let startp = startp in
    let endp = endp in
    { startp; endp }

(* Returns the start and end points of our input segment -- Input type segment -- Output type point * point *)
let get_points { startp; endp } = 
    (startp, endp)


(* A.2 *)

(* First rectangle type with lower left and upper right points *)
type rectangle = { ll : point; ur : point }

(* Creates a rectangle from lower left and upper right points -- Input type point point -- Output type rectangle *)
let make_rectangle ll ur = 
    let ll = ll in
    let ur = ur in
    { ll; ur }

(* Returns the segment that forms the lower part of our rectangle -- Input type rectangle -- Output type segment *)
let rectangle_lower_segment { ll; ur } = 
    make_segment ll (make_point ur.x ll.y)

(* Returns the segment that forms the upper part of our rectangle -- Input type rectangle -- Output type segment *)
let rectangle_upper_segment { ll; ur } = 
    make_segment (make_point ll.x ur.y) ur

(* Returns the segment that forms the left part of our rectangle -- Input type rectangle -- Output type segment *)
let rectangle_left_segment { ll; ur } = 
    make_segment (make_point ll.x ur.y) ll

(* Returns the segment that forms the right part of our rectangle -- Input type rectangle -- Output type segment *)
let rectangle_right_segment { ll; ur } = 
    make_segment ur (make_point ur.x ll.y)

(* Returns the perimeter of our rectangle -- Input type rectangle -- Output type float *)
let rectangle_perimeter rect = 
    ( (segment_length (rectangle_lower_segment rect) ) *. 2.) +.
    ( (segment_length (rectangle_left_segment rect) ) *. 2.)

(* Returns the area of our rectangle -- Input type rectangle -- Output type float *)
let rectangle_area rect = 
    (segment_length (rectangle_lower_segment rect) ) *. 
    (segment_length (rectangle_left_segment rect) )

(* Second rectangle type with floats representing upper and lower x and y values *)
type rectangle2 = { lx : float; ly : float; ux : float; uy : float }

(* Creates a rectangle2 from upper and lower x and y values -- Input type float float float float -- 
Output type rectangle2 *)
let make_rectangle2 lx ly ux uy = 
    let lx = lx and ly = ly and ux = ux and uy = uy in
    { lx; ly; ux; uy }

(* Returns the segment that forms the lower part of our rectangle2 -- Input type rectangle2 -- Output type segment *)
let rectangle_lower_segment2 { lx; ly; ux; uy } = 
    make_segment (make_point lx ly) (make_point ux ly)

(* Returns the segment that forms the upper part of our rectangle2 -- Input type rectangle2 -- Output type segment *)
let rectangle_upper_segment2 { lx; ly; ux; uy } = 
    make_segment (make_point lx uy) (make_point ux uy)

(* Returns the segment that forms the left part of our rectangle2 -- Input type rectangle2 -- Output type segment *)
let rectangle_left_segment2 { lx; ly; ux; uy } = 
    make_segment (make_point lx uy) (make_point lx ly)

(* Returns the segment that forms the right part of our rectangle2 -- Input type rectangle2 -- Output type segment *)
let rectangle_right_segment2 { lx; ly; ux; uy } = 
    make_segment (make_point ux uy) (make_point ux ly)

(* Returns the perimeter of our rectangle2 -- Input type rectangle2 -- Output type float *)
let rectangle_perimeter2 rect = 
    ( (segment_length (rectangle_lower_segment2 rect) ) *. 2.) +.
    ( (segment_length (rectangle_left_segment2 rect) ) *. 2.)

(* Returns the area of our rectangle2 -- Input type rectangle2 -- Output type float *)
let rectangle_area2 rect = 
    (segment_length (rectangle_lower_segment2 rect) ) *.
    (segment_length (rectangle_left_segment2 rect) )


(* A.3 *)

(* given functions *)
let make_pair x y = fun m -> m x y
(* Or, equivalently: let make_pair x y m = m x y *)
let first z = z (fun x y -> x)

(* Returns the second value of make_pair *)
let second z = z (fun x y -> y)

(* 
evaluate first (make_pair x y)
    evaluate make_pair x y
        evaluate x -> x
        evaluate y -> y
        evaluate make_pair -> fun x y -> fun m -> m x y
        apply make_pair to x y
            substitute x y for x y
    -> fun m -> m x y
    evaluate first -> fun z -> z (fun x y -> x)
    apply first to fun m -> m x y
        substitute fun m -> m x y for z
-> fun m -> m x y (fun x y -> x)
-> fun (fun x y -> x) -> (fun x y -> x) x y
-> fun x y -> x
-> x
*)

(* 
evaluate second (make_pair 1 2)
    evaluate make_pair 1 2
        evaluate 1 -> 1
        evaluate 2 -> 2
        evaluate make_pair -> fun x y -> fun m -> m x y
        apply make_pair to 1 2
            substitute 1 2 for x y -> (fun 1 2 -> fun m -> m 1 2)
    -> fun m -> (m 1 2)
    evaluate second -> fun z -> z (fun x y -> y)
    apply second to (fun m -> m 1 2)
        substitute (fun m -> m 1 2) for z -> (fun (fun m -> m 1 2) -> (fun m -> m 1 2) (fun x y -> y) )
    -> (fun m -> m 1 2) (fun x y -> y)
        evaluate (fun x y -> y) -> (fun x y -> y)
        evaluate (fun m -> m 1 2) -> (fun m -> m 1 2)
        apply fun m to (fun x y)
            substitute (fun x y -> y) for m -> (fun (fun x y -> y) -> (fun x y -> y) 1 2)
        -> (fun x y -> y) 1 2
            evaluate 1 -> 1
            evaluate 2 -> 2
            evaluate fun x y -> (fun x y -> y)
            apply fun x y to 1 2
                substitute 1 2 for x y -> (fun 1 2 -> 2)
            -> 2
-> 2
*)


(* A.4 *)

(* Recursive function that computes b to the n-th power (copied from lab3.ml) -- Input type int int --
Output type int *)
let rec pow b n = 
    match n with
        | 0 -> 1
        | _ -> b * pow b (n - 1)

(* This function utilizes an interative recursive helper to calculate the int_log of a given value 
from a given base -- Input type int int -- Output type int *)
let int_log b v =
    let rec int_log_helper b v curr =
        match v with 
            | _ when v mod b = 0 -> int_log_helper b (v / b) (curr + 1)
            | _ -> curr
    in
    int_log_helper b v 0

(* Creates our pair in the form of 2^a * 3^b -- Input type int int -- Output type int *)
let make_pairi a b = (pow 2 a) * (pow 3 b)

(* Gives the first value in our pairi (base 2) -- Input type int -- Output type int *)
let firsti z = int_log 2 z

(* Gives the second value in our pairi (base 3) -- Input type int -- Output type int *)
let secondi z = int_log 3 z


(* A.5 *)

(* The three functions below are given in specs *)
let zero = []

let is_zero = function
  | [] -> true
  | () :: _ -> false

let succ u = () :: u

(* Returns the input unary value - 1 -- Input type unit list -- Output type unit list *)
let prev = function (* can change below to [] *)
    | [] -> invalid_arg "Input cannot be 0"
    | _::t -> t

(* Converts an integer to an unary value -- Input type int -- Output type unit list *)
let rec integer_to_unary n =
    if n = 0 then zero else (succ (integer_to_unary (n - 1) ) )

(* Converts a unary value to an integer -- Input type unit list -- Output type int *)
let rec unary_to_integer u = 
    if (is_zero u) then 0 else ( (unary_to_integer (prev u) ) + 1)

(* Adds two unary values together -- Input type (unit list) (unit list) -- Output type unit list *)
let unary_add u1 u2 = 
    integer_to_unary ( (unary_to_integer u1) + (unary_to_integer u2) )

(* the four functions below were given in specs *)
type nat = Zero | Succ of nat

let zero' = Zero

let is_zero' = function
  | Zero -> true
  | Succ _ -> false

let succ' u = Succ u

(* Returns the input nat value - 1 -- Input type nat -- Output type nat *)
let prev' = function
    | Zero -> invalid_arg "Input cannot be 0"
    | Succ u -> u

(* It is clear that when considering integer_to_unary we now must utilize the Succ identifier as well as 
adding or subtracting 1 from calculated integers in order to take account for the representation of nat.
Most of the other code matches previous implementations. *)

(* Converts int to nat unary -- Input type int -- Output type nat *)
let rec integer_to_unary' n = 
    if (n = 0) then zero' else Succ (integer_to_unary' (n - 1) )

(* Converts nat unary to int -- Input type nat -- Output type int *)
let rec unary_to_integer' u =
    if (is_zero' u) then 0 else ( (unary_to_integer' (prev' u)) + 1)

(* Adds two nat unary values -- Input type nat nat -- Output type nat *)
let unary_add' u1 u2 = 
    Succ (integer_to_unary' ( (unary_to_integer' u1) + (unary_to_integer' u2) - 1) )


(* A.6 *)

(* zerof = "functional zero"; we call it this so as not to be confused with
   zero or zero' previously defined. The two following functions were given in the specs *)

let zerof = fun s -> fun z -> z
  (* or equivalently: let zerof = fun s z -> z *)
  (* or equivalently: let zerof s z = z *)

let add1 n = fun s -> fun z -> s (n s z)
  (* or equivalently: let add1 n = fun s z -> s (n s z) *)
  (* or equivalently: let add1 n s z = s (n s z) *)

(* The following represent the church numerals 1-10 *)
let one s z = s z
let two s z =  s (s z)
let three s z = s (s (s z) )
let four s z = s (s (s (s z ) ) )
let five s z = s (s (s (s (s z) ) ) )
let six s z = s (s (s (s (s (s z) ) ) ) )
let seven s z = s (s (s (s (s (s (s z) ) ) ) ) )
let eight s z = s (s (s (s (s (s (s (s z) ) ) ) ) ) )
let nine s z = s (s (s (s (s (s (s (s (s z) ) ) ) ) ) ) )
let ten s z = s( s (s (s (s (s (s (s (s (s z) ) ) ) ) ) ) ) )

(* Returns the sum of our two church numerals -- Input type church church ... -- Output 
type fun s z -> ... *)
let add m n s z = m s (n s z)

(* Converts the input church numeral to an integer -- Input type church -- Output type int *)
let church_to_integer c = 
    let add_helper m = m + 1 in
    c (add_helper) 0


(* A.7 *)

(*
Let us first consider church_to_integer zerof.  Here we have the following evaluation process:
evaluate church_to_integer zerof
    evaluate zerof -> fun s -> funz -> z
    evaluate church_to_integer -> fun ((int -> int) -> int -> 'c) -> 'c
    apply church_to_integer to zerof
        substitute zerof -> fun zerof -> 'c
        * therefore we can determine that the types in zerof will correspond to 'a = (fun int -> int),
          'b = int *
        * we can also determine that the second to last 'c in church_to_integer = 'b (int) since the output 
          of zerof is 'b *
        * thus, we can finally determine that the final output of our church_to_integer (shown as 'c)
          is also equal to 'b (int), and thus we return an integer, as desired *
        -> fun zerof -> int
    -> int
-> int

Now, let us consider church_to_integer one.  Here we have the following evaluation process:
evaluate church_to_integer one
    evaluate one -> fun ('a -> 'b) -> 'a -> 'b
    evaluate church_to_integer -> fun ((int -> int) -> int -> 'c) -> 'c
    apply church_to_integer to one
        substitute one -> fun one -> 'c
        * therefore we can determine that the types in one will correspond to 'a = int, 'b = int *
        * we can also determine that the second to last 'c in church_to_integer = 'b (int) since the output
          of one is 'b *
        * thus, we can finally determine that the final output of our church_to_integer (shown as 'c)
          is also equal to 'b (int), and thus we return an integer, as desired *
        -> fun one -> int
    -> int
-> int
*)



(** Section B **)
(* B.1 *)

(* the following five statements were given in the specs *)
type mobile = Mobile of branch * branch  (* left and right branches *)
and branch =
  | Weight    of int * int     (* length and weight *)
  | Structure of int * mobile  (* length and sub-mobile *)

let make_mobile l r = Mobile (l, r)
let make_weight l w = Weight (l, w)
let make_structure l m = Structure (l, m)

(* B.1.a *)

(* Return left branch of mobile -- Input type mobile -- Output type branch *)
let left_branch (Mobile (l, _)) = l

(* Return right branch of mobile -- Input type mobile -- Output type branch *)
let right_branch (Mobile (_, r)) = r

(* Return branch length -- Input type branch -- Output type int *)
let branch_length = function
    | Weight (len, _) -> len
    | Structure (len, _) -> len

(* Return branch structure -- Input type branch -- Output type [< `Weight (int) | `Structure (mobile)] *)
let branch_structure = function
    | Weight (_, wei) -> `Weight (wei)
    | Structure (_, mob) -> `Structure (mob)

(* B.1.b *)

(* Return branch weight -- Input type branch -- Output type [< int | mobile] *)
(* Return mobile weight -- Input type mobile -- Output type int *)
let rec branch_weight1 = function
    | Weight (_, wei) -> wei
    | Structure (_, mob) -> total_weight1 mob
    and total_weight1 (Mobile (l, r)) =
    (branch_weight1 l) + (branch_weight1 r)

(* Return branch weight (only abstraction functions) -- Input type branch -- Output type [< int | mobile] *)
(* Return mobile weight (only abstraction functions) -- Input type mobile -- Output type int *)
let rec branch_weight2 b = 
    match (branch_structure b) with
    | `Weight (wei) -> wei
    | `Structure (mob) -> total_weight2 mob
    and total_weight2 mob =
        let l = left_branch mob and r = right_branch mob in
    (branch_weight2 l) + (branch_weight2 r)

(* B.1.c *)

(* Return whether the mobile weight is balanced -- Input type mobile -- Output type bool *)
let rec is_balanced mob = 
    begin
    let l = left_branch mob and r = right_branch mob in
    let branch_lw b = (branch_length b) * (branch_weight2 b) in
    (branch_lw l) = (branch_lw r) && (ib_branch_iter l) && (ib_branch_iter r)
    end
    and ib_branch_iter b = 
        match (branch_structure b) with
        | `Weight (wei) -> true
        | `Structure (mob) -> is_balanced mob
    
(* B.1.d *)

(* The following three statements were given in the specs *)
type mobile'  = { left: branch'; right: branch' }
and  branch'  = Branch' of int * contents
and  contents = Weight' of int | Structure' of mobile'

(* Construct a mobile' -- Input type branch' branch' -- Output type mobile' *)
let make_mobile' l r = 
    let left = l and right = r in
    { left; right }

(* Construct a weighted branch' -- Input type int int -- Output type branch' *)
let make_weight' l w = 
    let contents = Weight' (w) in
    Branch' (l, contents)

(* Construct a structure branch' -- Input type int mobile' -- Output type branch' *)
let make_structure' l m = 
    let contents = Structure' (m) in
    Branch' (l, contents)

(* Return the left branch -- Input type mobile' -- Output type branch' *)
let left_branch' { left; right } = left

(* Return the right branch -- Input type mobile' -- Output type branch' *)
let right_branch' { left; right } = right

(* Return the branch length -- Input type branch' -- Output type int *)
let branch_length' (Branch' (l, _)) = l

(* Return the branch structure -- Input type branch' -- Output type [< `Weight (int) | `Structure (mobile')] *)
let branch_structure' (Branch' (_, c)) = 
    match c with 
    | Weight' (wei) -> `Weight (wei)
    | Structure' (mob) -> `Structure (mob)

(* Return the branch weight -- Input type branch' -- Output type [< int | mobile'] *)
(* Return mobile weight -- Input type mobile' -- Output type int *)
let rec branch_weight' b =
    match (branch_structure' b) with
    | `Weight (wei) -> wei
    | `Structure (mob) -> total_weight' mob
    and total_weight' mob =
        let l = left_branch' mob and r = right_branch' mob in
    (branch_weight' l) + (branch_weight' r)

(* Return whether mobile weight is balanced -- Input type mobile -- Output type bool *)
let rec is_balanced' mob = 
    begin
    let l = left_branch' mob and r = right_branch' mob in
    let branch_lw b = (branch_length' b) * (branch_weight' b) in
    (branch_lw l) = (branch_lw r) && (ib_branch_iter' l) && (ib_branch_iter' r)
    end
    and ib_branch_iter' b = 
        match (branch_structure' b) with
        | `Weight (wei) -> true
        | `Structure (mob) -> is_balanced' mob

(* for testing 
let m0 =
  make_mobile'
    (make_weight' 1 1)
    (make_weight' 1 1)

let m1 =
  make_mobile'
    (make_weight' 3 4)
    (make_structure'
      4
      (make_mobile'
        (make_weight' 1 2)
        (make_weight' 2 1)))

let m2 =
  make_mobile'
    (make_weight' 1 400)
    (make_structure'
      10
      (make_mobile'
        (make_weight' 100 1)
        (make_weight' 1 200)))

let m3 =
  make_mobile'
    (make_weight' 1 (total_weight' m2))
    (make_structure' 1 m2)
*)


(* B.2 *)

(* tree and elem types given in specs *)
type tree = Tree of elem list
and elem =
  | Num of int
  | Sub of tree

(* Returns the input tree with all numbers squared -- Input type tree -- Output type tree *)
let rec square_tree (Tree (elem)) = 
    Tree (st_helper elem)
    and st_helper = function
    | [] -> []
    | (Num (n) )::r -> (Num (n * n) ) :: (st_helper r)
    | (Sub (t) )::r -> (Sub (square_tree t) ) :: (st_helper r)

(* Returns the input tree with all numbers squared (using List.map) -- Input type tree -- Output type tree *)
let rec square_tree' (Tree (elem)) = 
    let map_fun = function
        | Num (n) -> Num (n * n)
        | Sub (t) -> Sub (square_tree' t) 
        in
    Tree (List.map (map_fun) elem)

(* for testing *)
(*
let tree1 = Tree
  [Num 10;
   Sub (Tree [Num 20;
              Sub (Tree [Num 42; Sub (Tree []); Num 12]);
              Sub (Tree []);
              Sub (Tree [Num 13; Sub (Tree [])])]);
   Sub (Tree []);
   Sub (Tree [Num 1; Num 2; Num 3])]

let tree2 = Tree
  [Num 100;
   Sub (Tree [Num 400;
              Sub (Tree [Num 1764; Sub (Tree []); Num 144]);
              Sub (Tree []);
              Sub (Tree [Num 169; Sub (Tree [])])]);
   Sub (Tree []);
   Sub (Tree [Num 1; Num 4; Num 9])]*)


(* B.3 *)

(* Applies the given function to our tree using List.map -- Input type tree -- Output type tree *)
let rec tree_map f (Tree (elem)) = 
    let map_fun = function
        | Num (n) -> Num (f n)
        | Sub (t) -> Sub (tree_map f t) 
        in
    Tree (List.map (map_fun) elem)



(** Section C **)

(* expr type given in specs *)
type expr =
  | Int of int           (* constant *)
  | Var of string        (* variable *)
  | Add of expr * expr   (* expr1 + expr2 *)
  | Mul of expr * expr   (* expr1 * expr2 *)
  | Pow of expr * int    (* expr^n *)

(* C.1 *)

(* Recursively simplifies input expression -- Input type expr -- Output type expr *)
(* simplify function attached is given in specs *)
let rec simplify1 = function
    | Add (Int (i1), Int (i2) ) -> Int (i1 + i2)
    | Mul (Int (i1), Int (i2) ) -> Int (i1 * i2)
    | Pow (Int (i1), i2) -> Int (pow i1 i2)
    | Add (Int (0), e) | Add (e, Int (0) ) -> e
    | Mul (Int (0), e) | Mul (e, Int (0) ) -> Int (0)
    | Mul (Int (1), e) | Mul (e, Int (1) ) -> e
    | Pow (e, 0) -> Int (1)
    | Pow (e, 1) -> e
    | Add (e1, e2) -> Add ( (simplify e1), (simplify e2) )
    | Mul (e1, e2) -> Mul ( (simplify e1), (simplify e2) )
    | Pow (e, i) -> Pow ( (simplify e), i)
    | e -> e
    and simplify expr =
        let e = simplify1 expr in
            if expr = e
            then expr
            else simplify e


(* C.2 *)

(* Recursively calculates the derivative of the input expression -- Input type expr -- Output type expr *)
let rec deriv var = function
    | Int (i) -> Int (0)
    | Var (v) when v = var -> Int (1)
    | Var (_) -> Int (0)
    | Add (e1, e2) -> Add ( (deriv var e1), (deriv var e2) )
    | Mul (e1, e2) -> Add ( (Mul ( (deriv var e1), e2) ), (Mul (e1, (deriv var e2) ) ) )
    | Pow (e, i) -> Mul ( (Mul (Int (i), Pow (e, (i - 1) ) ) ), (deriv var e) )

(* given function from specs *)
let derivative var expr =
  let e = simplify expr in
  let d = deriv var e in
    simplify d