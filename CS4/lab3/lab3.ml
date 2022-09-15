(*** Lab 3 - Dallas Taylor ***)

(** Section A **)
(* A.1 *)

(* 
This function takes a non-empty list as an argument and returns a list containing only its last element.
Parameters:
    _ - ('a list) non-empty list
Output - ('a list) containing last element of _
*)
let rec last_sublist = function
    | [] -> invalid_arg "last_sublist: empty list"
    | [last_e] -> [last_e]
    | l::next_l -> (last_sublist next_l)


(* A.2 *)

(*
This function takes a list as an argument and returns a list with the elements in reversed order.
Parameters:
    _ - ('a list)
Output - ('a list) reversed _
*)
let reverse in_list = 
    let rec iter_reverse_helper accum = function 
    | [] -> accum
    | first::rest -> (iter_reverse_helper (first::accum) rest)
    in
    iter_reverse_helper [] in_list


(* A.3 *)

(* 
This function takes a list of ints as an argument and returns a list with the square of each int entry.
Parameters:
    _ - (int list)
Output - (int list) list with each entry of _ squared
*)
let rec square_list = function
  | [] -> []
  | h :: t -> (h * h) :: (square_list t)

(* 
This function takes a list of ints as an argument and returns a list with the square of each int entry using
List.map functionality.
Parameters:
    items - (int list)
Helpers:
    square - (fun int -> int) returns square of input
Output - (int list) list with each entry of _ squared
*)
let square_list2 items = 
let square x = x * x in
List.map square items


(* A.4 *)

(* The reason that Louis' list is returned in the reverse order is because of the way that he is combining
our h^2 value and the cumulative answer.  Since we are using (h * h) :: answer, we will continue to add 
each not squared value to the front of our answer list, so that we eventually have the square of the first 
element at the last index and the square of the last element at the first index. *)

(* The second version of this function does not work because the answer value is of type 'a list and not 'a.  
Thus, we are attempting to insert a list to the front of a list and not a value of type 'a to the front of 
the list. *)

(* We can fix Louis' square_list function by changing the second match line with the following:
    | h :: t -> iter t (answer @ [(h * h)])

Thus, we are now appending the two lists together, as we have also changed our (h * h) value into an entry 
in its own list.

However, once we add the append operator @, we lose our desired time efficiency since we are not utilizing 
a non-tail recursive call.
*)


(* A.5 *)

(* 
This function takes a list of ints as an argument and returns a count of the number of negative ints using
an iterative recursive process.
Parameters:
    items - (int list)
Helpers:
    cnn_iter_helper - (fun int a' list -> int) returns count of negative entries
Output - (int) count of negative entries
*)
let count_negative_numbers items = 
    let rec cnn_iter_helper count = function
        | [] -> count
        | n::rest when n < 0 -> cnn_iter_helper (count + 1) rest
        | p::rest -> cnn_iter_helper count rest
    in
    cnn_iter_helper 0 items


(* A.6 *)

(* 
This function takes an int n as an argument and returns a list containing each power of 2 as an entry from i = 0
to i = n - 1.
Parameters:
    n - (int)
Helpers:
    pow_iter - (fun int int -> int) returns the first int to the power of the second int
    potl_helper - (fun int list int -> int list) iterates through each value in our output list and finds 2^n
Output - (int) list of length n with entries = 2 to increasing powers from 0 to n-1
*)
let power_of_two_list n =
    let rec pow_iter a b =
    match b with
        | 0 -> 1
        | _ -> a * pow_iter a (b - 1)
    in
    let rec potl_helper accum = function
        | -1 -> accum
        | n -> potl_helper ((pow_iter 2 n) :: accum) (n - 1)
    in
    potl_helper [] (n - 1)


(* A.7 *)

(* 
This function takes an int list as an argument and returns a list where the i-th element of the output list 
represents the sum of all of the values up to the i-th element in the input list.
Parameters:
    items - (int list)
Helpers:
    ps_iter_helper - (fun int int list -> int list) iteratively recurses through the input list, adding the 
    aggregate sum at each i to the front of the list
Output - (int list) 
*)
let prefix_sum items = 
    let rec ps_iter_helper curr accum = function
        | [] -> accum
        | first::rest -> ps_iter_helper (first + curr) (first + curr :: accum) rest
    in
    reverse (ps_iter_helper 0 [] items)


(* A.8 *)

(* 
This function takes a list of lists as its argument and returns the lst with the list elements reversed as well 
as the elements of each list reversed.
Parameters:
    items - ('a list list)
Helpers:
    dr_iter_helper - (fun 'a list list 'a list list -> 'a list list) iteratively recurses through the input list, 
    reversing the contents of each entry as we add that entry to the front of the list
Output - ('a list list) deep-reversed input list
*)
let deep_reverse items = 
    let rec dr_iter_helper accum = function
        | [] -> accum
        | first::rest -> dr_iter_helper ((reverse first)::accum) rest
    in
    dr_iter_helper [] items


(* A.9 *)

type 'a nested_list =
  | Value of 'a
  | List of 'a nested_list list

(*
This function takes an 'a nested list as its input and performs a deep_reverse on that list.
Parameters:
    _ - ('a nested_list)
Helpers:
    drn_iter_helper - (fun 'a list list 'a list list -> 'a list list) iteratively recurses through the input list, 
    reversing the contents of each entry as we add that entry to the front of the list
Output - ('a nested_list) deep-reversed input list 
*)
let rec deep_reverse_nested =
    let rec drn_iter_helper accum = function
        | [] -> accum
        | flst::rest -> drn_iter_helper ((deep_reverse_nested flst)::accum) rest
    in
    function
    | Value curr -> Value curr
    | List curr -> List (drn_iter_helper [] curr)



(** Section B **)
(* B.1 *)

let rec filter predicate sequence =
  match sequence with
    | [] -> []
    | h :: t when predicate h -> h :: filter predicate t
    | _ :: t -> filter predicate t

(*
This function takes in an 'a list and returns the list with entries sorted in an order defined by cmp.
    Parameters:
        cmp - (bool operator) such as <, >
        _ - ('a list) list to sort
    Output - ('a list) _ sorted with entries in ascending order
*)
let rec quicksort lst cmp =
    match lst with
    | [] -> []
    | first::rest ->  (quicksort (filter (fun b -> cmp b first) rest) cmp) @
                         (first::(quicksort (filter (fun b -> not (cmp b first)) rest) cmp))


(* B.2 *)

(* The quicksort function is an instance of generative recursion and not structural recursion 
because we as we iterate through our input list, our recursive calls for the next "layer" will have 
new inputs that were generated or calculated from the original input list. Thus, we are not simply 
iterating through each element in the list, but we are creating new sublists to iterate through that
are not natural occurring sublists. *)


(* B.3 *)

(* Ben's merge_sort function will not work because if we passed in a single element list, we would 
see that once this single element list is passed into our even_half function, we will begin an infinitely 
recursing loop.  This is because the even half list will be [] and our odd half list will be (self).  Thus,
as we recurse through using merge_sort, we continually pass in even half lists of [] and odd half lists of
(self), and thus we enter an inifinitely recursing loop. *)

(* B.4 *)

(* Given functions from B.4 with modified final line that causes our functions to implement a linear 
recursive process *)
let rec insert_in_order new_result a_list cmp =
  match a_list with
    | [] -> [new_result]
    | h :: t when cmp new_result h -> new_result :: a_list
    | h :: t ->  h :: insert_in_order new_result t cmp

let rec insertion_sort a_list cmp =
  match a_list with
    | [] -> []
    | h :: t -> insert_in_order h (insertion_sort t cmp) cmp

(* The above functions represent structural recursion.  This is due to the fact that we recurse over natural
subsets of our data and do not calculate a new subset as our next recursive input. *)



(** Section C **)
(* C.1 *)

(*
This function takes in a list (set) as input and returns a list of all possible subsets.
Parameters:
    _ - ('a list) list representation of our original set
Output - ('a list list) list of lists representation of possible subsets of _
*)
let rec subsets = function
  | [] -> [[]]
  | h :: t -> let rest = subsets t in
      rest @ (List.map (fun new_t -> h::new_t) rest)

(* This function works because as we loop through the first element of our input set, we recursively
call our subsets function on the remaining elements (calling this rest), and append that to the front 
of a list that maps the current first element (h) to the front of every element in rest.  Thus, if we 
considered and example input set such as [1; 2], we follow this process:
    [1; 2;] -> 1::[2;] -> rest = subsets [2;] in
        rest @ (List.map (fun new_t -> 1::new_t) rest) (hold as we calculate rest)
            rest = subsets [2;] -> 2::[] -> rest = subsets [] in 
                rest @ (List.map (fun new_t -> 2::new_t) rest) (hold as we calculate rest)
                    rest = subsets [] -> [[]]
                -> [[]] @ (List.map (fun new_t -> 2::new_t) [[]])
                -> [[]] @ [[2]]
                -> [[]; [2]]
            rest = [[]; [2]]
        -> [[]; [2]] @ (List.map (fun new_t -> 1::new_t) [[]; [2]])
        -> [[]; [2]] @ [[1]; [1; 2]]
        -> [[]; [2]; [1]; [1; 2]]

(I know that this was not required but it helped me to understand how the function worked more!)
*)


(* C.2 *)

(* Given functions for C.2 with various lines filled in *)
let rec accumulate op initial sequence =
  match sequence with
    | [] -> initial
    | h :: t -> op h (accumulate op initial t)

(* Maps single input function p to the elements of the list in sequence using accumulate *)
let map p sequence =
  accumulate (fun x r -> (p x)::r) [] sequence

(* Appends seq 1 to seq2 (in that order) using accumulate *)
let append seq1 seq2 =
  accumulate (fun x r -> x :: r) seq2 seq1

(* Calculates the length of a given sequence using accumulate *)
let length sequence =
  accumulate (fun x r -> 1 + r) 0 sequence


(* C.3 *)

(* 
This function takes in an operator, initial state, and a list of lists and returns the list with all of its 
list elements having been accumulated as desired.
Parameters: 
    op - (fun x y -> something) operator to perform while accumulating
    init - ('a) initial state
    seqs - ('a list list) list of lists to have accumulated
Output - seqs with each list element accumulated
 *)
let rec accumulate_n op init seqs =
  match seqs with
    | [] -> failwith "empty list"
    | [] :: _ -> []   (* assume all sequences are empty *)
    | h :: t -> accumulate op init ( List.hd h :: map List.hd t) :: 
        accumulate_n op init ( List.tl h :: map List.tl t)


(* C.4 *)

(* Given functions for C.4 with various lines filled in *)
let rec map2 f x y =
  match (x, y) with
    | ([], []) -> []
    | ([], _) -> failwith "unequal lists"
    | (_, []) -> failwith "unequal lists"
    | (h1::t1, h2::t2) -> (f h1 h2)::(map2 f t1 t2)

(* Calculates the dot product between two vectors using map2 *)
let dot_product v w = accumulate (+) 0 (map2 ( * ) v w)

(* Calculates a matrix times a vector using dot_product *)
let matrix_times_vector m v = map (dot_product v) m

(* Calculates the transpose of a matrix using accumulate_n *)
let transpose mat = accumulate_n (fun a b -> a::b) [] mat

(* Calculates a matrix times another matrix using transpose and matrix_times_vector *)
let matrix_times_matrix m n =
  let cols = transpose n in
     map (matrix_times_vector cols) m
