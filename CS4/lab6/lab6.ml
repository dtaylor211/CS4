(*** Lab 6 - Dallas Taylor ***)

(** Section A **)
(* A.1 *)

(* let factorial n =
  let rec iter m r =
    if m = 0
      then r
      else iter (m - 1) (r * m)
  in iter n 1
in
  factorial 3 *)

(*
  FRAME 0 (initial environment)
    parent: none
    bindings:
      - : [primitive function -]
      * : [primitive function *]

  FUNCTION 0 (fun n -> let rec iter m r ...)
    env: FRAME 0
    param: n
    body: let rec iter m r ...

  FRAME 1 (let factorial = FUNCTION 0 in ...)
    parent: FRAME 0
    bindings:
      factorial : FUNCTION 0
 
  FRAME 2 (FUNCTION 0 applied to 3)
    parent: FRAME 0
    bindings:
      n : 3
 
  FUNCTION 1 (fun m r -> if m = 0 ...)
    env: FRAME 2
    param: m r
    body: if m = 0 ...

  FRAME 3 (let rec iter = FUNCTION 1 in ...)
    parent: FRAME 2
    bindings:
      rec iter = FUNCTION 1

  FRAME 4 (FUNCTION 1 applied to 3 1)
    parent: FRAME 3
    bindings:
      m : 3
      r : 1

  FRAME 5 (FUNCTION 1 applied to 2 3)
    parent: FRAME 3
    bindings:
      m : 2
      r : 3

  FRAME 6 (FUNCTION 1 applied to 1 6)
    parent: FRAME 3
    bindings:
      m : 1
      r : 6

  FRAME 7 (FUNCTION 1 applied to 0 6)
    parent: FRAME 3
    bindings:
      m : 0
      r : 6
*)


(* A.2 *)

(* factorial function using references that does not use let rec *)
let factorial =
  let f = ref (fun n -> 0) in
  begin
  f := function
    | 0 -> 1;
    | n -> n * !f (n - 1);
  end;
  !f



(** Section B **)
(* B.1 *)

(* defining new exception *)
exception Stat_error of string

(* 
1st method for making a statistic 
  Methods:
  - append (adds a statistic)
  - mean (finds the mean of current statistics)
  - variance (finds the variance of current statistics)
  - stdev (finds the stdev of current statistics)
  - clear (resets all variables)
*)
let make_stat_1 () =
  let sum = ref 0. in
  let sumsq = ref 0. in
  let n = ref 0 in
    object
      method append a = 
        sum := !sum +. a;
        sumsq := !sumsq +. (a *. a);
        n := !n + 1;
      method mean = 
      match !n with
        | 0 -> raise (Stat_error "need at least one value for mean");
        | i -> !sum /. (float_of_int i) ;
      method variance = 
      match !n with 
        | 0 -> raise (Stat_error "need at least one value for variance");
        | i -> (!sumsq -. (!sum *. !sum /. (float_of_int i) ) ) 
                  /. (float_of_int i);
      method stdev = 
      match !n with
        | 0 -> raise (Stat_error "need at least one value for stdev");
        | i -> sqrt ( (!sumsq -. (!sum *. !sum /. (float_of_int i) ) ) 
                  /. (float_of_int i) );
      method clear = 
        sum := 0.;
        sumsq := 0.;
        n := 0;
    end

(* B.2 *)

(* 
2nd method for making a statistic 
Methods:
  - append (adds a statistic)
  - mean (finds the mean of current statistics)
  - _variance *private* (finds the variance of current statistics)
  - variance (gives _variance)
  - stdev (gives sqrt of _variance)
  - clear (resets all variables)
*)
let make_stat_2 () =
  let sum = ref 0. in
  let sumsq = ref 0. in
  let n = ref 0 in
    object (self)
      method append a = 
        sum := !sum +. a;
        sumsq := !sumsq +. (a *. a);
        n := !n + 1;
      method mean = 
      match !n with
        | 0 -> raise (Stat_error "need at least one value for mean");
        | i -> !sum /. (float_of_int i) ;
      method private _variance = 
        (!sumsq -. (!sum *. !sum /. (float_of_int !n) ) ) /. (float_of_int !n);
      method variance = 
      match !n with 
        | 0 -> raise (Stat_error "need at least one value for variance");
        | i -> self#_variance
      method stdev = 
      match !n with
        | 0 -> raise (Stat_error "need at least one value for stdev");
        | i -> sqrt self#_variance;
      method clear = 
        sum := 0.;
        sumsq := 0.;
        n := 0;
    end


(** Section C **)
(* C.1 *)

(* given in notes *)
module type PRIORITY_QUEUE =
  sig
    exception Empty

    type elem      (* Abstract type of elements of queue. *)
    type t         (* Abstract type of queue. *)

    val empty      : t                (* The empty queue.         *)
    val is_empty   : t -> bool        (* Check if queue is empty. *)
    val insert     : t -> elem -> t   (* Insert item into queue.  *)
    val find_min   : t -> elem        (* Return minimum element.  *)
    val delete_min : t -> t           (* Delete minimum element.  *)
    val from_list  : elem list -> t   (* Convert list to queue.   *)
  end

(* Priority Queue module implementation *)
module PriorityQueue : (PRIORITY_QUEUE with type elem = int) =
  struct
    exception Empty

    type elem = int

    (*
     * Data type: either
     * -- a Leaf, or
     * -- a Node of (rank, item, left heap, right heap).
     *)
    type t = Leaf | Node of int * elem * t * t

    let empty = Leaf
    let is_empty pq = pq = empty
    let find_min = function
      | Leaf -> raise Empty
      | Node (_, min, _, _) -> min
    let find_rank = function
      | Leaf -> 0
      | Node (r, _, _, _) -> r
    let make_pqueue min pq1 pq2 = 
        let r1 = find_rank pq1 and r2 = find_rank pq2 in
          if r1 < r2 then Node ( (r1 + 1), min, pq2, pq1) else
          Node ( (r2 + 1), min, pq1, pq2)
    let rec merge pq1 pq2 = match (pq1, pq2) with
      | (pq, Leaf) | (Leaf, pq) -> pq
      | (Node (_, min1, lh1, rh1), Node (_, min2, lh2, rh2)) -> 
          if min1 < min2 then make_pqueue min1 lh1 (merge rh1 pq2) else
          make_pqueue min2 lh2 (merge rh2 pq1)
    let insert pq item = merge pq (Node (1, item, empty, empty) )
    let delete_min = function
      | Leaf -> raise Empty
      | Node (_, _, lh, rh) -> merge lh rh
    let rec from_list = function
      | [] -> empty
      | h::t -> insert (from_list t) h
  end

  (* heap sort implementation using PriorityQueue *)
  let heap_sort lst = 
    let new_pq = PriorityQueue.from_list lst in
    let rec hs_helper pq result = 
      if PriorityQueue.is_empty pq then result 
      else hs_helper (PriorityQueue.delete_min pq) 
        ( (PriorityQueue.find_min pq)::result) 
    in
    List.rev (hs_helper new_pq [])


(* C.2 *)

(* Type for ordered comparisons. *)
type comparison = LT | EQ | GT

(* Signature for ordered objects. *)
module type ORDERED =
  sig
    type t
    val cmp: t -> t -> comparison
  end

(* given in specs *)
module OrderedString =
  struct
    type t = string
    let cmp x y =
      if x = y then EQ else if x < y then LT else GT
  end

(* MakePriorityQueue module implementation *)
module MakePriorityQueue (Elt : ORDERED)
  : (PRIORITY_QUEUE with type elem = Elt.t) =
  struct
    exception Empty
    type elem = Elt.t
    type t = Leaf | Node of int * elem * t * t

    let empty = Leaf
    let is_empty pq = pq = empty
    let find_min = function
      | Leaf -> raise Empty
      | Node (_, min, _, _) -> min
    let find_rank = function
      | Leaf -> 0
      | Node (r, _, _, _) -> r
    let make_pqueue min pq1 pq2 = 
        let r1 = find_rank pq1 and r2 = find_rank pq2 in
          if r1 < r2 then Node ( (r1 + 1), min, pq2, pq1) else
          Node ( (r2 + 1), min, pq1, pq2)
    let rec merge pq1 pq2 = match (pq1, pq2) with
      | (pq, Leaf) | (Leaf, pq) -> pq
      | (Node (_, min1, lh1, rh1), Node (_, min2, lh2, rh2)) -> 
          if (Elt.cmp min1 min2) = LT then make_pqueue min1 lh1 (merge rh1 pq2)
          else make_pqueue min2 lh2 (merge rh2 pq1)
    let insert pq item = merge pq (Node (1, item, empty, empty) )
    let delete_min = function
      | Leaf -> raise Empty
      | Node (_, _, lh, rh) -> merge lh rh
    let rec from_list = function
      | [] -> empty
      | h::t -> insert (from_list t) h
  end

(* given in specs *)
module StringPQ = MakePriorityQueue(OrderedString)

(* heap sort implementation using StringPQ *)
let heap_sort_2 lst = 
  let new_pq2 = StringPQ.from_list lst in
    let rec hs2_helper pq result = 
      if StringPQ.is_empty pq then result 
      else hs2_helper (StringPQ.delete_min pq) 
        ( (StringPQ.find_min pq)::result) 
    in
    List.rev (hs2_helper new_pq2 [])



(** Section D **)
(* D.1 *)

(* following code is given in specs and filled in for lazy implementation *)
type 'a contents = 
  | Expr of (unit -> 'a)
  | Val of ('a)
type 'a lazy_t = 'a contents ref

let make_lazy e = ref (Expr (e) )
let force lz = match !lz with
  | Expr e -> lz := Val (e () ); e ()
  | Val v -> v


(* C.2 *)

(* given in specs *)
let y =
  fun f ->
    (fun z -> z (`Roll z))
    (fun (`Roll w) -> f (fun x -> w (`Roll w) x))

(* C.2.a *)

(* 
let almost_sum = function
  | [] -> 0
  | h::t -> h + almost_sum t
*)

(* calculates the sum of list of strings using Y combinator *)
let sum = y (fun almost_sum -> 
  function 
  | [] -> 0
  | h::t -> h + (almost_sum t) )

(* C.2.b *)

(* calculates the factorial of n using Y combinator *)
let factorial2 n = y (fun iter -> 
  function
  | (0, r) -> r
  | (n, r) -> iter ( (n - 1), (n * r) ) ) (n, 1)