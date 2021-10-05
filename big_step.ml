#!/usr/bin/env ocaml

type expr =
  | ETrue
  | EFalse
  | EIf of expr * expr * expr
  | EZero
  | ESucc of expr
  | EPred of expr
  | EIsZero of expr

let rec show = function
  | ETrue ->
     "true"
  | EFalse ->
    "false"
  | EIf (cond, e1, e2) ->
    Printf.sprintf "(If (%s, %s, %s))"
        (show cond)
        (show e1)
        (show e2)
  | EZero ->
    "EZero"
  | ESucc e ->
    Printf.sprintf "(Succ %s)" (show e)
  | EPred e ->
    Printf.sprintf "(Pred %s)" (show e)
  | EIsZero e ->
     Printf.sprintf "(IsZero %s)" (show e)

exception Stuck of expr

let rec is_num_val = function
  | EZero ->
    true
  | ESucc n ->
    is_num_val n
  | _ ->
    false

let is_value = function
    | ETrue | EFalse -> true
    | _ as e -> is_num_val e

let rec eval e = match e with
  (*B-VALUE*)
  | v when is_value v ->
    v
  | EIf (t1, t2, t3) ->
    begin match eval t1 with
    (*B-IFTRUE*)
    | ETrue ->
      eval t2
    (*B-IFFALSE*)
    | EFalse ->
      eval t3
    | _ -> raise (Stuck e)
    end
  (*B-Succ*)
  | ESucc n ->
    let nv = eval n in
    if is_num_val nv then
      ESucc nv
    else
      raise (Stuck e)
  | EPred t1 ->
    begin match eval t1 with
    (*B-PREDZERO*)
    | EZero ->
        EZero
    (*B-PREDSUCC*)
    | ESucc nv1 when is_num_val nv1 ->
        nv1
    | _ ->
      raise (Stuck e)
    end
  | EIsZero t1 ->
    begin match eval t1 with
    (*B-ISZEROZERO*)
    | EZero ->
        ETrue
    | ESucc nv1 when is_num_val nv1 ->
        EFalse
    | _ -> raise (Stuck e)
    end
  | _ ->
    raise (Stuck e)

let test e expected =
  let res = eval e in 
  if res = expected then
      Printf.printf "pass %s evaluates to %s\n" (show e) (show (eval e))
  else
      Printf.printf "FAIL %s: expected %s but got %s\n" (show e) (show expected) (show res)

let test_fail e =
  match eval e with
    | exception Stuck stuck ->
          Printf.printf "pass %s gets stuck: %s\n" (show e) (show stuck)
    | value ->
      Printf.printf "FAIL expected Stuck exception but got %s\n" (show value)

let _ =
  test EZero EZero;
  test (ESucc EZero) (ESucc EZero);
  test (EPred (ESucc (ESucc EZero))) (ESucc EZero);
  test (EIf ((EIsZero (EPred (ESucc EZero))), EZero, ETrue)) EZero;
  test_fail (EIf ((EIsZero (EPred (EPred ETrue))), EZero, ETrue));
  test_fail (EIf ((EIsZero (ESucc ETrue)), EZero, ETrue));
