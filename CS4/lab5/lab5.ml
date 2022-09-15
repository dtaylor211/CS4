(*** Lab 5 - Dallas Taylor ***)

(** Section A **)
(* A.1 *)

(* Input - (int) n
    Output - (int) the n-th fibonacci number 
    While Loop *)
let fibonacci n =
    let m = ref n and prev = ref 0 and curr = ref 1 in
    if !m < 2 then !m else
    begin
    while !m > 1 do
        let curr_i = !curr in
        curr := !curr + !prev;
        prev := curr_i;
        m := !m - 1
    done;
    !curr
    end

(* Input - (int) n
    Output - (int) the n-th fibonacci number 
    For Loop *)
let fibonacci2 n =
    let prev = ref 0 and curr = ref 1 in
    if n < 2 then n else 
    begin
    for i = 2 to n do
        let curr_i = !curr in
        curr := !curr + !prev;
        prev := curr_i
    done;
    !curr
    end


(* A.2 *)

(* Input - (Array) arr
    Output - (Array) arr sorted in ascending order *)
let bubble_sort arr = 
    let all_correct = ref false in
    while not !all_correct do
        all_correct := true;
        begin
        for i = 0 to ( (Array.length arr) - 2) do
        if arr.(i) > arr.(i + 1) then 
        let arri_i = arr.(i) in 
        arr.(i) <- arr.(i + 1); 
        arr.(i + 1) <- arri_i;
        all_correct := false
        done;
        end
    done



(** Section B **)
(* B.1 *)

(* meters per foot conversion *)
let meters_per_foot = 0.3048

(* get meter length of measurement *)
let get_meters len =
  match len with
    | `Meter m -> m
    | `Foot f -> f *. meters_per_foot
    | `Inch i -> (i *. meters_per_foot) /. 12.0

(* add two lengths *)
let length_add a b = `Meter (get_meters a +. get_meters b)


(* B.2 *)

(* grams per slug conversion *)
let grams_per_slug = 14593.903203

(* get gram weight of measurement *)
let get_grams wei = 
  match wei with
    | `Gram g -> g
    | `Kilo k -> k *. 1000.0
    | `Slug s -> s *. grams_per_slug

(* add two masses *)
let mass_add a b = `Gram (get_grams a +. get_grams b)

(* get second time of measurement *)
let get_seconds t = 
  match t with
    | `Second s -> s
    | `Minute m -> m *. 60.0
    | `Hour h -> h *. 3600.0
    | `Day d -> d *. 24.0 *. 3600.0

(* add two times *)
let time_add a b = `Second (get_seconds a +. get_seconds b)


(* B.3 *)

(* adds two units together if they are compatible *)
let unit_add a b = match (a, b) with
    | (`Length l1, `Length l2) -> `Length (length_add l1 l2)
    | (`Mass m1, `Mass m2) -> `Mass (mass_add m1 m2)
    | (`Time t1, `Time t2) -> `Time (time_add t1 t2)
    | _ -> failwith "units must be compatible for addition"

(* We do not get into a combinatorial explosion as we add more unit classes. 
This is due to the fact that we can clearly see from the above that each 
additional unit class only requires one new line in our function.  All other
cases are not relevant (as they are not compatible) and thus our final line 
remains unchanged. *)


(*** Section C ***)
(* C.1 *)

let rec make_gram g =
  let grams_per_slug = 14593.903203 in
  let compatible_h other = 
    other#unit_type = `Gram || other#unit_type = `Slug in
  let add_h other = if (compatible_h other) 
                    then make_gram(g +. other#get_grams)
                    else failwith "units must be compatible for addition" in
    object
      method get_grams = g
      method get_slugs = g /. grams_per_slug
      method unit_type = `Gram
      method compatible other = compatible_h other
      method add other = add_h other
    end


(* C.2 *)

(* Given Code Below *)
(* Define a number as a message-passing object. *)
(* "i" is an int. *)
let rec make_number i =
  object
    method value = i
    method show = string_of_int i
    method is_zero = i = 0
    method is_number = true
    method evaluate _ _ = make_number i  (* must evaluate to an object *)
    method derive _ = make_number 0  (* derivative of a number is 0 *)
  end

(* Define a variable as a message-passing object. *)
(* "v" is a string. *)
let rec make_variable v =
  object
    method value = failwith "variable has no numerical value"
    method show  = v
    method is_zero = false
    method is_number = false
    method evaluate v' n =
      if v = v'
        then make_number n
        else make_variable v
    method derive v' =
      if v = v'
        then make_number 1  (* d/dx(x) = 1 *)
        else make_number 0  (* d/dx(y) = 0 *)
  end

(* Define a sum as a message-passing object. *)
let rec make_sum expr1 expr2 =
  match () with
    | _ when expr1#is_zero -> expr2  (* 0 + expr = expr *)
    | _ when expr2#is_zero -> expr1  (* expr + 0 = expr *)
    | _ when expr1#is_number && expr2#is_number ->  (* add numbers *)
          make_number (expr1#value + expr2#value)
    | _ ->  (* create a new object representing the sum *)
          object
            method value = failwith "sum expression has no numerical value"
            method show = "(" ^ expr1#show ^ " + " ^ expr2#show ^ ")"
            method is_zero = false
            method is_number = false
            method evaluate v n =
              make_sum (expr1#evaluate v n) (expr2#evaluate v n)
            method derive v =
              make_sum (expr1#derive v) (expr2#derive v)
          end

(* Evaluate a message-passing expression with a number
   substituted for a variable. *)
let evaluate expr v n = expr#evaluate v n

(* Return the string representation of an expression. *)
let show expr = expr#show

(* Return the derivative of an expression. *)
let differentiate expr v = expr#derive v

(* C.2.a *)

let rec make_product expr1 expr2 = 
  match () with 
    | _ when expr1#is_zero || expr2#is_zero -> make_number 0
    | _ when expr1#is_number && expr2#is_number -> 
        make_number (expr1#value * expr2#value)
    | _ when expr1#is_number && expr1#value = 1 -> expr2 
    | _ when expr2#is_number && expr2#value = 1 -> expr1
    | _ -> 
        object 
            method value = failwith "product expression has no numerical value"
            method show = "(" ^ expr1#show ^ " * " ^ expr2#show ^ ")"
            method is_zero = false
            method is_number = false
            method evaluate v n = 
                make_product (expr1#evaluate v n) (expr2#evaluate v n)
            method derive v = make_sum (make_product (expr1#derive v) (expr2) )
                                       (make_product (expr2#derive v) (expr1) )
        end

(* C.2.b *)
(* C.2.b.i *)
(* val f :
  < derive : string -> 'a; evaluate : string -> int -> 'a; is_number : 
    bool; is_zero : bool; show : string; value : int >
  as 'a = <obj> *)

(* C.2.b.ii *)
(* val dfdx :
  < derive : string -> 'a; evaluate : string -> int -> 'a; is_number : 
    bool; is_zero : bool; show : string; value : int >
  as 'a = <obj> *)

(* C.2.b.iii *)
(* - : string =
"(((x * (x * y)) + (((x * y) + (y * x)) * x)) + 
(((x * (y * y)) + ((y * y) * x)) * 3))"
 *)

(* C.2.b.iv *)
(* - : string =
"((3 * (3 * (3 * y))) + ((3 * (3 * (3 * (y * y)))) + ((y * y) + 2)))" *)

(* C.2.b.v *)
(* - : string = "558" *)

(* C.2.b.vi *)
(* - : string = "396" *)
