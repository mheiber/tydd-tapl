
module Ctx = Map.Make (String)
module Vars = Set.Make(String)


type ty =
  | TyBool
  | TyArr of ty * ty
  | TyUnit

type expr =
  | EUnit
  | ETrue
  | EFalse
  | EIf of expr * expr * expr
  | EVar of string
  | EAbs of string * ty * expr
  | EApp of expr * expr
  | ESeq of expr * expr

exception Type_Mismatch of (*expected*) ty * (*got*) ty
exception Expected_Fun_Type of (*got*) ty
exception Shadowed of string

let rec show_ty = function
  | TyUnit -> "unit"
  | TyBool -> "bool"
  | TyArr (e1, e2) -> Printf.sprintf "(%s -> %s)" (show_ty e1) (show_ty e2)

let e_children = function
  | EUnit | ETrue | EFalse | EVar _ -> []
  | EIf (e1, e2, e3) ->
    [e1; e2; e3]
  | EAbs(_, _, e) ->
    [e]
  | EApp(e1, e2) ->
    [e1; e2]
  | ESeq(e1, e2) ->
    [e1; e2]

let validate_no_shadow e =
  let rec aux vars e = match e with
  | EUnit | ETrue | EFalse | EIf _ | EVar _ | EApp _ | ESeq _ ->
    List.iter (aux vars) (e_children e)
  | EAbs(x, _ty, _e) when Vars.mem x vars ->
    raise (Shadowed x)
  | EAbs(x, _ty, e) ->
    let vars = Vars.add x vars in
    aux vars e;
  in
  aux Vars.empty e

let type_of e =
  let rec aux ctx = function
  | EUnit ->
    TyUnit
  | ETrue | EFalse ->
    TyBool
  | EIf(cond, branch1, branch2) ->
    begin match aux ctx cond with
    | TyBool ->
      let branch1_ty = aux ctx branch1 in
      let branch2_ty = aux ctx branch2 in
      if branch1_ty == branch2_ty then
        branch1_ty
      else
          raise (Type_Mismatch(branch1_ty, branch2_ty))

    | other ->
      raise (Type_Mismatch(TyBool, other))
    end
   | EVar s ->
         Ctx.find s ctx
   | EAbs(x, ty, expr) ->
     let ctx = Ctx.add x ty ctx in
     aux ctx expr
   | EApp(e1, e2) ->
     let t2 = aux ctx e2 in
     begin match aux ctx e1 with
     | TyArr(domain, codomain) when domain == t2 ->
       codomain
     | TyArr(domain, _codomain) ->
          raise (Type_Mismatch(domain, t2))
     | other ->
          raise (Expected_Fun_Type(other))
     end
   | ESeq(e1, e2) ->
      let _ = aux ctx e1 in
      aux ctx e2
  in
  validate_no_shadow e;
  aux Ctx.empty e

 
let assert_ty e expected_ty =
  let actual_ty = type_of e in
  if actual_ty <> expected_ty then 
    failwith (Printf.sprintf "got %s, expected %s" (show_ty actual_ty) (show_ty expected_ty))

let check_fails_shadow e x =
  match type_of e with
    | exception (Shadowed x') when x = x' -> ()
    | ty -> failwith (Printf.sprintf "expected exception for shadowing, got %s" (show_ty ty))

let _ =
  assert_ty ETrue TyBool;
  assert_ty (EIf(ETrue, ETrue, ETrue)) TyBool;
  let app e = EAbs("f", TyArr(TyBool, TyBool), e) in
  let use = (EApp(EVar"f", ETrue)) in
  assert_ty (app use) TyBool;
  check_fails_shadow (app (app use)) "f";
  assert_ty EUnit TyUnit;
  assert_ty (ESeq(EUnit, ETrue)) TyBool;
