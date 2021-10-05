#!/usr/bin/env ocaml

type expr =
  | ETrue
  | EFalse
  | EIf of expr * expr * expr
  | EZero
  | ESucc of expr
  | EPred of expr
  | EIsZero of expr


let rec is_number = function
  | EZero -> true
  | ESucc e -> is_number e
  | _ -> false

let rec is_val = function
  | ETrue | EFalse | EZero ->
    true
  | EIf _ | ESucc _  | EPred _ | EIsZero _ ->
    false

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
    "0"
  | ESucc e ->
    Printf.sprintf "(Succ %s)" (show e)
  | EPred e ->
    Printf.sprintf "(Pred %s)" (show e)
  | EIsZero e ->
     Printf.sprintf "(IsZero %s)" (show e)



exception Stuck of string * string

let rec eval_num = function
  | EIf (cond, e1, e2) ->
        if eval_bool cond then
          eval_num e1
        else
          eval_num e2
  | EZero ->
        0
  | EPred (ESucc n) ->
        eval_num n
  | ESucc n ->
        (eval_num n) + 1
  | (ETrue | EFalse | EPred _ | EIsZero _) as e ->
        raise (Stuck ("eval_num", (show e)))

and eval_bool = function
  | ETrue ->
    true
  | EFalse ->
    false
  | EIf(ETrue, e1, _e2) ->
        eval_bool e1
  | EIf(EFalse, _e1, e2) ->
        eval_bool e2
  | EIf(cond, e1, e2) ->
        if eval_bool cond then
          eval_bool e1
        else
          eval_bool e2
  | EIsZero e ->
        if eval_num e = 0 then true else false
  | (EZero | ESucc _ | EPred _) as e ->
        raise (Stuck ("eval_bool", (show e)))

let test e expected =
  let res = eval_num e in 
  if res = expected then
      Printf.printf "pass %s evaluates to %d\n" (show e) (eval_num e)
  else
      Printf.printf "FAIL %s: expected %d but got %d\n" (show e) expected res

let test_fail e =
  match eval_num e with
    | exception Stuck(where, what) ->
          Printf.printf "pass %s gets stuck: %s %s\n" (show e) where what
    | value ->
      Printf.printf "FAIL expected Stuck exception but got %d\n" value

let _ =
  test EZero 0;
  test (ESucc EZero) 1;
  test (EPred (ESucc (ESucc EZero))) 1;
  test (EIf ((EIsZero (EPred (ESucc EZero))), EZero, ETrue)) 0;
  test_fail (EIf ((EIsZero (EPred (EPred EZero))), EZero, ETrue));
  test_fail (EIf ((EIsZero (ESucc EZero)), EZero, ETrue));

