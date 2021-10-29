let tru t f = t
let fls t f = f
let test l m n = l m n
let band b1 b2 = b1 b2 fls
let bor b1 b2 = b1 tru b2
let not b = b fls tru

let pair a b = fun f -> f a b  
let fst pair = pair (fun a _ -> a)
let snd pair = pair (fun _ b -> b)


let zero = fun _ z -> z
let one = fun s z ->
  s z

let two = fun s z -> s (s z)
let three = fun s z -> s (s (s z))

let succ n =
    fun s z ->
        s (n s z)

module type Test = sig
  val run : unit -> unit
end


(* let to_nat church_nat = church_nat ((+) 1) 0 *)
let to_nat church_nat = church_nat ((+) 1) 0

let rec of_nat = function
  | 0 -> zero
  | n -> succ @@ of_nat (n - 1)

let to_list church_nat = church_nat (fun lst -> () :: lst) []

let rec of_list = function
  | [] -> zero
  | _ :: tl -> succ @@ of_list tl

let exp got expected =
  if got = expected then
    print_endline "pass"
  else 
    Printf.printf "fail expected: %d, but got: %d\n" expected got

let assert_eq church nat =
  exp  (to_nat church) nat

let assert_eq_l church lst =
  assert (to_list church = lst);
  assert (to_list(of_list lst) = to_list church);
  ()

let _ =
  assert_eq zero 0;
  assert_eq_l zero [];
  assert_eq one 1;
  assert_eq_l one [()];
  assert_eq two 2;
  assert_eq_l two [(); ()];
  ()
(* 5.2.2: find another way to define successor *)
(* my boring solution: may not actually be right.
 * Is there a rule that there is a distinguished zero?
 * *)
let _succ_v2 n =
    fun s _ ->
        s (n s zero)

(* more interesting solution from appendix *)
let succ_v3 n =
    fun s z ->
        n s (s z)

let _ =
  let s = succ_v3 in
  assert_eq (s zero) 1;
  assert_eq (s (s zero)) 2;
  assert_eq (s (s (s zero))) 3;
  assert_eq_l (s zero) [()];
  assert_eq_l (s (s zero)) [(); ()];
  assert_eq_l (s (s (s zero))) [(); (); ()];
  ()

let plus m n =
    fun s z ->
        m s (n s z)

let plus_v2 m n =
  m (succ n) zero
  (* fun s z -> *)
  (*   n s (m s z) *)
    

let mult m n =
    m (plus n) zero

let mult_v2 m n =
    m (n succ) zero
    
let test_plus (+) =
  assert_eq (zero + succ) 1;
  assert_eq (one + zero) 1;
  assert_eq (one + one) 2;
  assert_eq (two + one) 3;
  ()

let test_mult ( * ) =
  assert_eq (zero * one) 0;
  assert_eq (one * zero) 0;
  assert_eq (one * one) 1;
  assert_eq (two * one) 2;
  assert_eq (two * two) 4;
  assert_eq (two * three) 6;
  ()

let _ =
  test_plus plus;
  test_mult mult;
  test_plus plus_v2;
  (* test_mult mult_v2; *)
  ()

        
(* let _ = *)
(*   assert ((test tru tru fls) == tru) *)


