(** Problem Set 2 - Dallas Taylor **)
open Num

(** Section A **)
(* A.1 *)

(* The space complexity of our recursive Fibonacci function is O(n).  This is because as we calculate fib n,
we will have pending operations as we calculate fib(n-1), which is thus n-1 total pending operations at maximum.
This is different from time complexity as we are only considering the max space that is taken up in computation
at any point in time. *)


(* A.2 *)

(* A.2.1 *)
(* The p function is applied 5 times when calculating sin 12.15.  This is due to the fact that 12.15 requires 
being divided by 3 a total of 5 times in order to produce an angel that is less than 0.1. *)

(* A.2.2 *)
(* When evaluating sine a, we can see that the order of growth will be O(log_3 a) = O(log a).  This is due to 
the fact that it is clear that our input a will continue using recursion until we have shown that (a / (3)^n) 
is less than 0.1 for n steps of recursion. *)


(* A.3 *)

(* A.3.a *)
(* This function calculates the n-th exponent of b.
    Parameters:
        b - (int) base of our exponent
        n - (int) power to be raised to
    Helpers:
        is_even - (int -> bool) whether int is even
        square - (int -> int) computes int * int
    Output - (int) b^n
*)
let rec fast_expt b n =
  let is_even m = m mod 2 = 0 in
  let square m = m * m in
    match n with
      | 0 -> 1
      | _ when is_even n -> square (fast_expt b (n / 2) )
      | _ -> b * fast_expt b (n - 1)

(* A.3.b *)
(* This function uses an iterative recursive process to compute b ^ n through successive squaring.
    Parameters:
        b - (int) base of our exponent
        n - (int) power to be raised to
    Helpers:
        square - (int -> int) computes int * int
        is_even - (int -> bool) whether int is even
        ifast_expt_helper (int int int -> int) helper to compute b^n
    Output - (int) b^n
*)
let ifast_expt b n = 
    let square m = m * m in
    let is_even m = m mod 2 = 0 in
    let rec ifast_expt_helper b n a = 
        begin
        match n with
        | 0 -> a
        | _ when is_even n -> ifast_expt_helper (square b) (n / 2) a
        | _ -> ifast_expt_helper b (n - 1) (a * b)
        end
     in
        ifast_expt_helper b n 1


(* A.4 *)

(* This function computes the addition of two inputs using a logarithmic number of steps.
    Parameters:
        a - (int) first value to be multiplied
        b - (int) second value to be multiplied
    Helpers:   
        double - (int -> int) computes int * 2
        halve - (int -> int) computes int / 2
        is_even - (int -> bool) whether int is even
    Output - (int) a * b
*)
let rec fast_mult a b = 
    let double m = 2 * m in
    let halve m = m / 2 in
    let is_even m = m mod 2 = 0 in
      match b with
        | 0 -> 0
        | _ when is_even b -> double (fast_mult a (halve b))
        | _ -> a + (fast_mult a (b - 1))


(* A.5 *)

(* This function computes the addition of two inputs using an iterative recursive process.
    Parameters:
        a - (int) first value to be multiplied
        b - (int) second value to be multiplied
    Helpers:   
        double - (int -> int) computes int * 2
        halve - (int -> int) computes int / 2
        is_even - (int -> bool) whether int is even
    Output - (int) a * b
*)
let ifast_mult a b = 
    let double m = m * 2 in
    let halve m = m / 2 in
    let is_even m = m mod 2 = 0 in
    let rec ifast_mult_helper a b s = 
        begin
        match b with 
            | 0 -> s
            | _ when is_even b -> ifast_mult_helper (double a) (halve b) s
            | _ -> ifast_mult_helper a (b - 1) (s + a)
        end in
    ifast_mult_helper a b 0


(* A.6 *)

(* Under the assumption that f operates in constant time and space complexity, we can determine that foo can
compute in O(log n) space complexity and O(n) time complexity.   This is due to the fact that for some given n, that we will
have (at the worst case scenario) as many as l = log(n) pending operations 2^l = n. However, we have O(n) time complexity 
since we also double the number of total operations needed to be completed at each step. *)


(* A.7 *)

(* A.7.1 *)
(* This function represents a linear recursive process.  This is due to the fact that let statements are 
evaluated in the order of <expr> <name> <body>. Thus, we have pending operations on each level (not tail 
recursion), indicative of a linear recursive process. *)

(* A.7.2 *)
(* This function has a time and space complexity of O(n).  This is due to the fact that our steps increase
linearly, along with our time complexity.  All calls other than last_two operate in constant time. *)



(** Section B **)
(* B.1 *)

(* B.1.a *)
(* 
This function can be desugared into the following:

(fun x y -> x * (2 + y)) 20 (2 * 4)
*)

(* B.1.b *)
(* 
This function can be desugared into the following:

(fun a b c -> sqrt (b *. b -. 4.0 *. a *. c)) 1.0 20.0 3.0  
*)

(* B.1.c *)
(* 
This function can be desugared into the following:

(fun x -> let y = 2 in let z = 3 in x * y * z) 1
(fun x -> (fun y -> let z = 3 in x * y * z) 2) 1
(fun x -> (fun y -> (fun z -> x * y * z) 3) 2) 1
*)

(* B.1.d *)
(*
This function can be desugared into the following:

(fun x -> let x = 2 in let x = 3 in x * x * x) 1
(fun x -> (fun x -> let x = 3 in x * x * x) 2) 1
(fun x -> (fun x -> (fun x -> x * x * x) 3) 2) 1
*)


(* B.2 *)

(*
This function can be desugared and evaluated in the following order:

Desugar:
(fun x y -> let y = 14 in let z = 22 in x * y * z) (2 * 10) (3 + 4)
(fun x y -> (fun y -> let z = 22 in x * y * z) 14) (2 * 10) (3 + 4)
(fun x y -> (fun y -> (fun z -> x * y * z) 22) 14) (2 * 10) (3 + 4)

Evaluate -> (fun x y -> (fun y -> (fun z -> x * y * z) 22) 14) (2 * 10) (3 + 4)
    Evaluate 2 * 10 -> 20 (skipping intermediate steps)
    Evaluate 3 + 4 -> 7
    Evaluate (fun x y -> (fun y -> (fun z -> x * y * z) 22) 14)
        Evaluate 14
        Evaluate fun y -> (fun z -> x * y * z) 22
            Evaluate 22
            Evaluate (fun z -> x * y * z)
            Apply fun z to 22
                Substitute 22 for z
            -> fun 22 -> x * y * 22
        Apply fun y to 14
            Substitute 14 for y
        -> fun 14 -> x * 14 * 22
    Apply fun x y to 20 7
        Substitute 20 7 for x y
    -> fun 20 7 -> 20 * 14 * 22
-> 6160
*)


(* B.3 *)
(* 
Let us first show how our let looks when desugared:
(fun x y z -> x + y + z) 10 (x * 2) (y + 3)

This code gives an error since x is not considered defined in this instance.  This is due to 
the fact that we will evaluate the arguments to our expression first, which include 10, 
(x * 2), and (y + 3). Since we have not defined anything as x prior to this function, we will
get an error that x is unbounded.

A simple way to fix this error would be to change our let statement into a nested let statement:
let x = 10 in
let y = x * 2 in
let z = y + 3 in
x + y + z
(Here we pass in a value for x before calculating x * 2)
*)



(** Section C **)
(* This function converts int to an equivalent num type *)
let ni = num_of_int     (* convert int -> num *)

(* C.1 *)

(* This function calculates the sum across a specified sequence.
    Parameters:
        term -> (fun num -> num) calculates the term in a sequence given a sequence value
        a -> (num) starting sequence value
        next -> (fun num -> num) calculates the next value in the sequence
        b -> (num) ending sequence value
    Output - (num) sum of all values in the sequence defined by term and next from a to b
*)
let isum term a next b =
  let rec iter a result =
    if a >/ b
       then result
       else iter (next a) ( (term a) +/ result)
  in
    iter a (ni 0)


(* C.2 *)

(* C.2.1 *)
(* 
This function computes the product of values across a given sequence.
    Parameters:
        term -> (fun num -> num) calculates the term in a sequence given a sequence value
        a -> (num) starting sequence value
        next -> (fun num -> num) calculates the next value in the sequence
        b -> (num) ending sequence value
    Output - (num) product of all values in the sequence defined by term and next from a to b
*)
let rec product_rec term a next b = 
    if a >/ b
        then (ni 1)
        else term a */ (product_rec term (next a) next b)

(* 
This function uses our recursive product function to calculate the factorial of a number using a recursive process.
    Parameters:
        n - (num) number to take the factorial of
    Helpers:
        get_val - (fun num -> num) returns the given num
        next_val - (fun num -> num) returns the given num + 1
    Output - (num) factorial of n
*)
let factorial_rec n = 
let get_val m = m in
let next_val m = m +/ (ni 1) in
    product_rec get_val (ni 1) next_val n

(* infinite product expansion up to n terms
note that here I multiplied both sides of the equation by 2 to produce the fact that 
pi/2 = sum (4n^2/(4n^2 - 1)) *)
let pi_product n = 
let get_val m = ((m */ m) */ (ni 4)) // (((m */ m) */ (ni 4)) -/ (ni 1))  in
let next_val m = m +/ (ni 1) in
      (ni 2) */ product_rec get_val (ni 1) next_val n

(* defined in terms of pi_product *)
let pi_approx = float_of_num (pi_product (ni 1000))     

(* C.2.2 *)

(* 
This function computes the product of values across a given sequence using an iterative recursive process.
    Parameters:
        term -> (fun num -> num) calculates the term in a sequence given a sequence value
        a -> (num) starting sequence value
        next -> (fun num -> num) calculates the next value in the sequence
        b -> (num) ending sequence value
    Output - (num) product of all values in the sequence defined by term and next from a to b
*)
let product_iter term a next b = 
    let rec product_iter_helper a result =
    if a >/ b
        then result
        else product_iter_helper (next a) ((term a) */ result) in
    product_iter_helper a (ni 1)

(* 
This function uses our iterative recursive product function to calculate the factorial of a number.
    Parameters:
        n - (num) number to take the factorial of
    Helpers:
        get_val - (fun num -> num) returns the given num
        next_val - (fun num -> num) returns the given num + 1
    Output - (num) factorial of n
*)
let factorial_iter n = 
let get_val m = m in
let next_val m = m +/ (ni 1) in
    product_rec get_val (ni 1) next_val n


(* C.3 *)

(* C.3.1 *)
(* 
This function is a generalized version of product and sum using a recursive process.
    Parameters:
        combiner - (fun num -> num) the way the sequence values are to be combined (either +/ or */)
        null_value - (num) the null value of our sequence (either (ni 0) or (ni 1))
        term -> (fun num -> num) calculates the term in a sequence given a sequence value
        a -> (num) starting sequence value
        next -> (fun num -> num) calculates the next value in the sequence
        b -> (num) ending sequence value
    Output - (num) product/sum of all values in the sequence defined by term and next from a to b
*)
let rec accumulate_rec combiner null_value term a next b = 
    if a >/ b
        then null_value
        else combiner (term a) (accumulate_rec combiner null_value term (next a) next b)

(* Thus, it is clear that we can clearly replicate our product and sum functions as the following: *)
let sum term a next b = 
    accumulate_rec (+/) (ni 0) term a next b

let product term a next b = 
    accumulate_rec ( */) (ni 1) term a next b

(* C.3.2 *)
(* 
This function is a generalized version of product and sum using an iterative recursive process.
    Parameters:
        combiner - (fun num -> num) the way the sequence values are to be combined (either +/ or */)
        null_value - (num) the null value of our sequence (either (ni 0) or (ni 1))
        term -> (fun num -> num) calculates the term in a sequence given a sequence value
        a -> (num) starting sequence value
        next -> (fun num -> num) calculates the next value in the sequence
        b -> (num) ending sequence value
    Output - (num) product/sum of all values in the sequence defined by term and next from a to b
*)
let accumulate_iter combiner null_value term a next b = 
let rec accumulate_iter_helper combiner a result = 
    if a >/ b
        then result
        else accumulate_iter_helper (combiner) (next a) (combiner (term a) result)
        in
    accumulate_iter_helper (combiner) a null_value


(* C.4 *)

(* 
This function computes the composition of two single input functions such as f ( g ( x ) ).
    Parameters:
        f - (fun (single_input) -> output) outside function to be applied
        f - (fun (single_input) -> (single output)) inside function to be applied
    Output - fun x -> f applied to g applied to x
*)
let compose f g x = 
    f (g x)


(* C.5 *)

(* 
This function recursively composes a function so as to apply it n times.
    Parameters:
        f - (fun (input) -> (output))
        n - (num) number of times to apply f
    Helpers:
        self - (fun int -> int) a function that returns its input
    Output - fun x -> f applied to f applied to ... to x
*)
let rec repeated f n = 
let self m = m in
    if n < 1
    then self
    else compose f (repeated f (n - 1))


(* C.6 *)

(* 
This function returns a function that returns the smoothed value of that function for some input.
    Parameters:
        dx -> (float) change in x (a small number)
        f - (fun float -> float) function to be smoothed
        x - (float) variable to be applied (can be left blank to create a function)
    Output - fun x -> smoothed value of f at x
*)
let smooth dx f x =
    (((f (x -. dx)) +. (f x)) +. (f (x +. dx))) /. (3.)


(* C.7 *)

(* 
This function returns a function that returns the n-smoothed value of that fucntion for some input.
    Parameters:
        dx -> (float) change in x (a small number)
        f - (fun float -> float) function to be smoothed
        n - (int) number of times to repeat smoothing
    Output - fun x -> n-smoothed value of f at x
*)
let nsmoothed dx f n = 
    (repeated (smooth dx) n) f



(** Section D **)
(* D.1 *)

(* 
This function recursively composes calculates whether a given int is a prime number.
    Parameters:
        n - (int) number to determine if prime
    Helpers:
        square - (fun int -> int) gives square of input
        is_prime_helper - (fun int int -> bool) recursively checks if a divisor is found or if we pass our 
            upper limit
    Output - (bool) true if n is prime, false otherwise
*)
let is_prime n =
let square m = m * m in
let rec is_prime_helper div n = 
    match n with 
    | _ when (square div) > n -> true
    | _ when n mod div = 0 -> false
    | _ -> is_prime_helper (div + 1) n
    in
if n < 2 then false
else is_prime_helper 2 n


(* D.2 *)

(* 
This function recursively composes calculates the smallest_prime_factor of a number.
    Parameters:
        n - (int) number to determine smallest prime factor
    Helpers:
        smallest_prime_factor_helper - (fun int -> int) recursively finds the smallest prime factor
    Output - (int) smallest prime factor
*)
let smallest_prime_factor n = 
let rec smallest_prime_factor_helper div = 
    match div with 
    | _ when is_prime div && n mod div = 0 -> div
    | _ -> smallest_prime_factor_helper (div + 1)
    in
    match n with
    | _ when n < 2 -> invalid_arg "Input must be larger than 1"
    | _ when is_prime n -> invalid_arg "Input cannot be a prime number"
    | _ -> smallest_prime_factor_helper 2
