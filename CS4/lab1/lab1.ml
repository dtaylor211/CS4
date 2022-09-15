(** Dallas Taylor - Lab 1 **)
(** Section A **)
(* A.1 *)

(* 1 *)
(* 10;; *)
(* - : int = 10 *)

(* 2 *)
(* 10.;; *)
(* - : float = 10. *)

(* 3 *)
(* 5 + 3 + 4;; *)
(* - : int = 12 *)

(* 4 *)
(* 3.2 + 4.2;;*) 
(* Error: This expression has type float but an expression was expected of type 
        int *)
(* This error occurred because the plain addition operator + requires two integer inputs 
    and 3.2 and 4.2 are both floats *)

(* 5 *)
(* 3 +. 4;;*)
(* Error: This expression has type int but an expression was expected of type 
        float 
        Hint: Did you mean `3.'?*)
(* This error occurred because this specific addition operator +. requires a float as
as its first input and 3 is an int, so it suggests converting to a float 3. *)

(* 6 *)
(* 3 + 4.2;; *)
(* Error: This expression has type float but an expression was expected of type 
        int *)
(* This error occurred because the plain addition operator + requires an int as 
its first input and 4.2 is a float *)

(* 7 *)
(* 3 +. 4.2;; *)
(* Error: This expression has type int but an expression was expected of type 
        float.  
        Hint: Did you mean `3.'?*)
(* This error occurred because this specific addition operator +. requires a float as
its first input and 3 is an int, so it suggests converting to a float 3. *) 

(* 8 *)
(* 3.0 +. 4.2;; results in  *)
(* - : float = 7.2 *)

(* 9 *)
(* 9 - 3 - 1;; *)
(* - : int = 5 *)

(* 10 *)
(* 9 - (3 - 1);; *)
(* - : int = 7 *)

(* 11 *)
(* let a = 3;; *)
(* val a : int = 3 *)

(* 12 *)
(* let b = a + 1;; *)
(* val b : int = 4 *)

(* 13 *)
(* a = b;; *)
(* - : bool = false *)

(* 14 *)
(* [1; 2; 3] = [1; 2; 3];; *)
(* - : bool = true *)

(* 15 *)
(* [1; 2; 3] == [1; 2; 3];; *)
(* - : bool = false *)
(* This is not the same as the previous expression since the == operator 
checks if two items are the same thing in memory, whereas the = operator
checks for structural equality *)

(* 16 *)
(* [(1, 2, 3)];; *)
(* - : (int * int * int) list = [(1, 2, 3)] *)

(* 17 *)
(* [1, 2, 3];; *)
(* - : (int * int * int) list = [(1, 2, 3)] *)
(* We get this result since OCaml interprets the commas (use semicolons 
for desired behavior) in our list as indicative of a tuple, and thus 
constructs a list with one 3-tuple. *)

(* 18 *)
(* if b > a && b < a * b then b else a;; *)
(* - : int = 4 *)
    
(* 19 *)
(* if b > a and b < a * b then b else a;; *)
(* Error: Syntax error *)  
(* This error occurred because OCaml does not recognize 'and' as an operator *)  

(* 20 *)
(* 2 + if b > a then b else a;; *)
(* - : int = 6 *)

(* 21 *)
(* if b > a then b else a + 2;; *)
(* - : int = 4 *)
(* We get this result since the + 2 is considered part of the else statement,
and thus is never reached and b is returned *)

(* 22 *)
(* (if b > a then b else a) + 2;; *)
(* - : int = 6 *)

(* 23 *)
(* if b > a then b;; *)
(* Error: This expression has type int but an expression was expected of type
         unit
       because it is in the result of a conditional with no else branch *)
(* This error occurred because OCaml assumes that the type of the else branch is unit. *)


(* A.2 *)

(*
This function returns the sum of the two largest integers given.
        Input i1 (int) i2 (int) i3 (int)
        Output int
*)
let sum_of_squares_of_two_largest i1 i2 i3 = 
if i1 > i3 && i2 > i3 then i1 * i1 + i2 * i2
    else 
        begin
            if i1 > i2 then i1 * i1 + i3 * i3 else i2 * i2 + i3 * i3
        end
    

(* A.3 *)

(* The given function checks if b is greater than 0, where if it is then it sets 
the operator of a and b to be +, else it sets the operator to be -, giving the absolute
value of b. *)



(** Section B **)
(* B.1 *)

(* Ben will observe that an applicative order evaluation interpreter will enter an infinite 
loop and he will not be able to evaluate anything else, as we will continually be re-evaluating 
our expression on the value we just produced since we evaluate the value of our arguments before our conditional. Normal 
order evaluation would result in 0 as we evaluate the conditional first, and then move directly to our "then" statement *)


(* B.2 *)

(* When Alyssa attempts to run her code, she will encounter a stack overflow due to looping
recursion.  This is because we will continually be evaluating the sqrt_iter function since 
we have are using new_if, which is a function and not a built in like if.  Thus, all of the
inputs to our new_if statement are evaluated (and thus we infinitely recurse through sqrt_iter) 
even if we reach our base case.  This problem would be resolved if we used the normal if then 
else format *)


(* B.3 *)

(* B.3.1 *)
(* Executing add_a 2 5 results in a recursive process. This is because add_a will cause intermediate
values to be "put on hold", as we recursively trace through the function. *)
(* Executing add_b 2 5 results in an iterative process. This is because add_b uses tail iteration (where 
the last thing we call is our recursive function) and thus we do not need to store intermediate values. *)

(* B.3.2 *)
(*
let rec add_a a b =
  if a = 0
     then b
     else inc (add_a (dec a) b)

Desugar this to:

let rec add_a =
  fun a b ->
    if a = 0
       then b
       else inc (add_a (dec a) b)

Bind the name "add_a" to the value:

  fun a b ->
    if a = 0
       then b
       else inc (add_a (dec a) b)

evaluate add_a 2 5
  evaluate 2 -> 2
  evaluate 5 -> 5
  evaluate add_a -> fun a b -> if a = 0 then b else inc (add_a (dec a) b)
  apply fun a b -> ... to 2 5
    substitute 2 5 for a b -> ...

  evaluate if 2 = 0 then 5 else inc (add_a (dec 2) 5)
    evaluate 2 = 0
      evaluate 2 -> 2
      evaluate 0 -> 0
      evaluate = -> = [primitive function]
      apply = to 2 0
    result -> false

    evaluate else inc (add_a (dec 2) 5)
      evaluate inc (add_a (dec 2) 5)
        evaluate add_a (dec 2) 5

          evaluate dec 2
            evaluate 2 -> 2
            evaluate dec -> dec [prim. function]
            apply dec [prim. function] to 2
          result -> 1
          evaluate 5 -> 5
          evaluate add_a -> fun a b -> ...
          apply fun a b -> ... to 1 5
            substitute 1 5 for a b -> ...

          evaluate if 1 = 0 then 5 else inc (add_a (dec 1) 5)
            evaluate 1 = 0
              evaluate 1 -> 1
              evaluate 0 -> 0
              evaluate = -> = [prim. function]
              apply = to 1 0
            result -> false

            evaluate else inc (add_a (dec 1) 5)
              evaluate inc (add_a (dec 1) 5)
                evaluate add_a (dec 1) 5

                  evaluate dec 1
                    evaluate 1 -> 1
                    evaluate dec -> dec [prim. function]
                    apply dec [prim. function] to 1
                  result -> 0
                  evaluate 5 -> 5
                  evaluate add_a -> fun a b -> ... (already listed)
                  apply fun a b -> ... to 0 5
                    substitute 0 5 for a b -> ... (on next line)

                  evaluate if 0 = 0 then 5 else inc (add_a (dec 0) 5)
                    evaluate 0 = 0
                      evaluate 0 -> 0
                      evaluate 0 -> 0
                      evaluate = -> = [prim. function]
                      apply = to 0 0
                    result -> true

                    evaluate then 5
                      evaluate 5 -> 5
                    result -> 5
                  result -> 5

                result -> 5
              evaluate inc -> inc [prim. function]
              apply inc [prim. function] to 5
            result -> 6
          result -> 6

        result -> 6
      result -> 6
      evaluate inc -> inc [prim. function]
      apply inc [prim. function] to 6
    result -> 7

  result -> 7

result -> 7
*)

(* B.3.3 *)
(*
let rec add_b a b =
  if a = 0
     then b
     else add_b (dec a) (inc b)

Desugar this to:

let rec add_b =
  fun a b ->
    if a = 0
       then b
       else add_b (dec a) (inc b)

Bind the name "add_b" to the value:

  fun a b ->
    if a = 0
       then b
       else add_b (dec a) (inc b)

Evaluate (add_b 2 5)
  >>> evaluate 2 -> 2
  >>> evaluate 5 -> 5
  >>> evaluate add_b -> fun a b -> ...
  apply (fun a b -> if ...) to 2, 5
  substitute 2 for a, 5 for b in (if ...)
    -> if 2 = 0 then 5 else add_b (dec 2) (inc 5)
  evaluate (if 2 = 0 then 5 else add_b (dec 2) (inc 5))
    if is a special form, so evaluate the first operand:
      evaluate (2 = 0)
        >>> evaluate 2 -> 2
        >>> evaluate 0 -> 0
        >>> evaluate = -> = [prim. function]
        apply = to 2, 0 -> false
    first argument of if is false, so evaluate the third operand:
      evaluate (add_b (dec 2) (inc 5))
        evaluate (dec 2)
          >>> evaluate 2 -> 2
          >>> evaluate dec -> dec [prim. function]
          apply dec to 2 -> 1
        evaluate (inc 5)
          >>> evaluate 5 -> 5
          >>> evaluate inc -> inc [prim. function]
          apply inc to 5 -> 6
        evaluate add_b -> fun a b -> ...
        apply (fun a b -> if ...) to 1, 6
        substitute 1 for a, 6 for b in (if ...)
          -> if 1 = 0 then 6 else add_b (dec 1) (inc 6)
        evaluate (if 1 = 0 then 6 else add_b (dec 1) (inc 6))
          if is a special form, so evaluate the first operand:
            evaluate (1 = 0)
              >>> evaluate 1 -> 1
              >>> evaluate 0 -> 0
              >>> evaluate = -> = [prim. function]
              apply = to 1, 0 -> false
          first argument of if is false, so evaluate the third operand:
            evaluate (add_b (dec 1) (inc 6))
              evaluate (dec 1)
                >>> evaluate 1 -> 1
                >>> evaluate dec -> dec [prim. function]
                apply dec to 1 -> 0
              evaluate (inc 6)
                >>> evaluate 6 -> 6
                >>> evaluate inc -> inc [prim. function]
                apply inc to 6 -> 7
              evaluate add_b -> fun a b -> ...
              apply (fun a b -> if ...) to 0, 7
              substitute 0 for a, 7 for b in (if ...)
                -> if 0 = 0 then 7 else add_b (dec 0) (inc 7)
              evaluate (if 0 = 0 then 7 else add_b (dec 0) (inc 7))
                if is a special form, so evaluate the first operand:
                  evaluate (0 = 0)
                    >>> evaluate 0 -> 0
                    >>> evaluate 0 -> 0
                    >>> evaluate = -> = [prim. function]
                    apply = to 0, 0 -> true
                first argument of if is true, so evaluate the second operand:
                  >>> evaluate 7 -> 7
                  result: 7
*)



(** Section C **)
(* C.1 *)

(* 
This function computes the factorial of the input number,
which for a number n is equal to n * (n-1) * ... * 1. 
        Input n (int) 
        Output int
*)
let rec factorial n =
  if n = 0 then 1 else n * factorial (n - 1)

(* C.1.a *)

(* 
This function computes the i-th entry of our infinite series expansion,
returns a float.
        Input i (int)
        Output float
*)
let e_term i = 
        1. /. (float_of_int (factorial i) )

(* C.1.b *)

(* 
This function computes the approximation of e using 1/(i!) from i = 0 to i = n 
        Input n (int)
        Output float
*)
let rec e_approximation n =
        if n = 0 
        then e_term 0
        else e_term n +. e_approximation (n - 1)

(* C.1.c *)

(* e_approximation 20;; *)
(* - : float = 2.71828182845904553 *)

(* exp 1.0 *)
(* - : float = 2.71828182845904509 *)

(* We can see that our answers are the same until the 15th decimal point, thus they are nearly identical *)

(* C.1.d *)

(* Running our e_approximation function with n = 100 results in - : float = infinity.  This occurs because
our factorial function begins to deal with very large numbers.  Once it starts to deal with numbers larger
than the allowed integer size, it eventually will start to return 0, resulting in a 1/0 term = infinity. *)


(* C.2 *)

(* 
Mutually recursive functions that returns a bool for whether a functions is even/odd.
Calling is_even (an even int) results in true and is_even (an odd int) results in false.
Calling is_odd (an even int) results in false and is_odd (an even int) results in true. 
        Input i (int)
        Output bool
*)
let rec is_even i = 
        if i = 0
        then true
        else is_odd (i - 1)
and is_odd i = 
        if i = 0
        then false
        else is_even (i - 1)


(* C.3 *)

(* 
This function uses a recursive process to solve for the function f(n) = f(n-1) + 2f(n-2) + 3f(n-3) 
        Input n (int)
        Output int
*)
let rec f_rec n = 
        if n < 3
        then n
        else f_rec (n - 1) + 2 * f_rec (n - 2) + 3 * f_rec(n - 3)

(* 
This function is a helper function for f_iter that uses an iterative process to solve for the function 
f(n) = f(n-1) + 2f(n-2) + 3f(n-3) 
        Input step (int) nm1 (int) nm2 (int) nm3 (int)
        Output int
*)
let rec f_iter_helper step nm1 nm2 nm3 = 
        if step < 3
        then nm1
        else (f_iter_helper (step - 1) (nm1 + (2 * nm2) + (3 * nm3)) nm1 nm2) 

(* 
This function calls our helper function f_iter_helper that uses an iterative process to solve for the function 
f(n) = f(n-1) + 2f(n-2) + 3f(n-3) 
        Input n (int)
        Output int
*)
let f_iter n = 
        if n < 3
        then n
        else f_iter_helper  n 2 1 0


(* C.4 *)

(* 
This function uses a recursive process to calculate elements of Pascal's triangle (starting at row = 1, column = 1).
Throws an error if the row or column input is out of range (row or column < 1, column > row).
        Input row (int) column (int)
        Output int
*)
let rec pascal_coefficient row column = 
        match column with
        | _ when column < 1 || column > row || row < 1 -> failwith "invalid arguments"
        | 1 -> 1
        | r when r = row -> 1
        | _ -> begin
        match row with 
        | 1 -> 1
        | _ -> pascal_coefficient (row - 1) (column - 1) + pascal_coefficient (row - 1) (column)
        end