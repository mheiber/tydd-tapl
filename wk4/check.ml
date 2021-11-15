module StrSet = Set.Make (String)
module StrMap = Map.Make(String)

let ty_eq t1 t2 = t1 = t2

type ty =
  | TyBool
  | TyArr of ty * ty
  | TyUnit
  | TyTuple of ty list
  | TyRecord of ty StrMap.t
  | TyVariant of ty StrMap.t

type expr =
  | EUnit
  | ETrue
  | EFalse
  | EIf of expr * expr * expr
  | EVar of string
  | EAbs of string * ty * expr
  | EApp of expr * expr
  | ESeq of expr * expr
  | EAs of expr * ty
  | ELet of string * expr * expr
  | ETuple of expr list
  | ETupleProj of expr * int
  | ERecord of expr StrMap.t
  | ERecordProj of expr * string
  | EVariant of (*ctor_name*) string * expr * (*variant type*)ty
  | ECase of expr * expr StrMap.t

exception Type_Mismatch of (*expected*) ty * (*got*) ty
exception Expected_Fun_Type of (*got*) ty
exception Expected_Tuple_Type of (*got*) ty
exception Expected_Record_Type of (*got*) ty
exception Expected_Variant_Type_W_Ctor of (*ctor*) (string * ty) * (*got*) ty
exception Expected_Variant_Type_W_Ctors of StrSet.t * (*got*) ty
exception Expected_Variant_Type of (*got*) ty
exception Shadowed of string

let rec show_ty = function
  | TyUnit -> "unit"
  | TyBool -> "bool"
  | TyArr (e1, e2) -> Printf.sprintf "(%s -> %s)" (show_ty e1) (show_ty e2)
  | TyTuple tys -> Printf.sprintf "(%s)" (String.concat ", " (List.map show_ty tys))
  | TyRecord fields ->
    let bindings = StrMap.bindings fields in
    Printf.sprintf "{%s}" (String.concat ", " (List.map show_binding bindings))
  | TyVariant ctors ->
    let ctors = StrMap.bindings ctors in
    Printf.sprintf "<%s>" (String.concat ", " (List.map show_binding ctors))
and show_binding (name, ty) = Printf.sprintf "%s = %s" name (show_ty ty)

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
  | EAs(e, _ty) ->
    [e]
  | ELet(x, e1, e2) ->
    [e1; e2]
  | ETuple es ->
    es
  | ETupleProj (e, int) ->
    [e]
  | ERecord fields ->
    let bindings = (StrMap.bindings fields) in
    let snd (_, v) = v in
    List.map snd bindings
  | ERecordProj(record, _) ->
    [record]
  | EVariant(name, e, ty) ->
    [e]
  | ECase(e, handlers) ->
    let snd (_, expr)  = expr in
    e :: (List.map snd (StrMap.bindings handlers))

let validate_no_shadow e =
  let rec aux vars e = match e with
  | EUnit | ETrue | EFalse | EIf _ | EVar _ | EApp _ | ESeq _
  | EAs _ | ETuple _ | ETupleProj _ | ERecord _ | ERecordProj _
  | EVariant _ | ECase _ ->
    List.iter (aux vars) (e_children e)
  | EAbs(x, _ty, _e) when StrSet.mem x vars ->
    raise (Shadowed x)
  | EAbs(x, _ty, e) ->
    let vars: StrSet.t = StrSet.add x vars in
    aux vars e;
  | ELet(x, e1, e2) ->
    aux vars e1;
    let vars = StrSet.add x vars in
    aux vars e2
  in
  aux StrSet.empty e

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
         begin match StrMap.find_opt s ctx with
         | Some ty ->
            ty
         | None ->
            failwith ("unbound " ^ s)
         end
   | EAbs(x, ty, expr) ->
     let ctx = StrMap.add x ty ctx in
     aux ctx expr
   | EApp(e1, e2) ->
     let t2 = aux ctx e2 in
     begin match aux ctx e1 with
     | TyArr(domain, codomain) when ty_eq domain t2 ->
       codomain
     | TyArr(domain, _codomain) ->
          raise (Type_Mismatch(domain, t2))
     | other ->
          raise (Expected_Fun_Type(other))
     end
   | ESeq(e1, e2) ->
      let _ = aux ctx e1 in
      aux ctx e2
    | EAs(e, expected_ty) ->
      let actual_ty = aux ctx e in
      if not (ty_eq expected_ty actual_ty) then
          raise (Type_Mismatch(expected_ty, actual_ty))
      else
        expected_ty
    | ELet(x, e1, e2) ->
      let ty = aux ctx e1 in
      let ctx = StrMap.add x ty ctx in
      aux ctx e2
    | ETuple es ->
      let tys = List.map (fun e -> aux ctx e) es in
      TyTuple tys
    | ERecord fields ->
      let bindings = StrMap.map (fun e -> aux ctx e) fields in
      TyRecord bindings
    | ETupleProj(e, index) ->
      begin match aux ctx e with
      | TyTuple tys ->
        List.nth tys index
      | ty ->
        raise (Expected_Tuple_Type ty)
      end
    | ERecordProj(e, field) ->
      begin match aux ctx e with
      | TyRecord fields ->
        StrMap.find field fields
      | ty ->
        raise (Expected_Record_Type ty)
      end
    | EVariant(ctor_name, e, variant_ty) ->
      let ctor_ty = aux ctx e in
      begin match variant_ty with
      | TyVariant bindings ->
        begin match StrMap.find_opt ctor_name bindings with
          | Some expected_ctor_ty when ty_eq expected_ctor_ty ctor_ty ->
            variant_ty
          | _ ->
            raise (Expected_Variant_Type_W_Ctor ((ctor_name, ctor_ty), variant_ty))
        end
      | _ ->
        raise (Expected_Variant_Type_W_Ctor((ctor_name, ctor_ty), variant_ty))
      end

    | ECase (variant, handlers) ->
      let fst (x, _) = x in
      let keys m = StrSet.of_seq @@ Seq.map fst @@ StrMap.to_seq m in
      let handler_ctors = keys handlers in
      begin match aux ctx variant with
      | (TyVariant bindings) as variant_ty ->
          let variant_ctors = keys bindings in
          if handler_ctors <> variant_ctors then
            raise (Expected_Variant_Type_W_Ctors(handler_ctors, variant_ty))
          else
            begin match StrMap.bindings handlers with
              | [] -> 
                (*could have a better bottom here probably*)
                variant_ty
              | (ctor_name, handler) :: tl ->
                let elab ctor_name handler = 
                  let binding_ty = StrMap.find ctor_name bindings in
                  let ctx = StrMap.add ctor_name binding_ty ctx in
                  aux ctx handler
                in
                let expected_ty = elab ctor_name handler in
                let check name handler =
                  let ty = elab name handler in
                  if not (ty_eq ty expected_ty) then
                    raise (Type_Mismatch(expected_ty, ty))
                  else
                    ()
                in begin
                  StrMap.iter check handlers;
                  expected_ty
                end
            end
      | other ->
        raise (Expected_Variant_Type_W_Ctors(handler_ctors, other))
      end
  in
  validate_no_shadow e;
  aux StrMap.empty e

 
let assert_ty e expected_ty =
  let actual_ty = type_of e in
  if actual_ty <> expected_ty then 
    failwith (Printf.sprintf "got %s, expected %s" (show_ty actual_ty) (show_ty expected_ty))

let check_fails_shadow e x =
  match type_of e with
    | exception (Shadowed x') when x = x' -> ()
    | ty -> failwith (Printf.sprintf "expected exception for shadowing, got %s" (show_ty ty))

let check_fails_ty_mismatch e =
  match type_of e with
    | exception (Type_Mismatch(_expected, _got)) -> ()
    | ty -> failwith (Printf.sprintf "expected exception for type mismatch, got %s" (show_ty ty))

let map_of_pairs pairs = StrMap.of_seq @@ List.to_seq pairs

let _ =
  assert_ty ETrue TyBool;
  assert_ty (EIf(ETrue, ETrue, ETrue)) TyBool;
  let app e = EAbs("f", TyArr(TyBool, TyBool), e) in
  let use = (EApp(EVar"f", ETrue)) in
  assert_ty (app use) TyBool;
  check_fails_shadow (app (app use)) "f";
  assert_ty EUnit TyUnit;
  assert_ty (ESeq(EUnit, ETrue)) TyBool;
  assert_ty (EAs(ETrue, TyBool)) TyBool;
  check_fails_ty_mismatch (EAs(ETrue, TyUnit));
  assert_ty (ELet("x", ETrue, EVar"x")) TyBool;
  let tuple = (ETuple [ETrue; EUnit; EFalse]) in
  assert_ty tuple (TyTuple [TyBool; TyUnit; TyBool]);
  assert_ty (ETupleProj(tuple, 0)) TyBool;
  assert_ty (ETupleProj(tuple, 1)) TyUnit;
  let record = ERecord (map_of_pairs [("a", ETrue); ("b", EUnit); ("c", EFalse)]) in
  assert_ty record (TyRecord (map_of_pairs [("a", TyBool); ("b", TyUnit); ("c", TyBool)]));
  assert_ty (ERecordProj(record, "a")) TyBool;
  assert_ty (ERecordProj(record, "b")) TyUnit;
  let variant = EVariant("a", ETrue, TyVariant(map_of_pairs [("a", TyBool); ("b", TyUnit)])) in
  let handlers = map_of_pairs [
    ("a", EIf(EVar"a", EUnit, EUnit));
    ("b", EVar"b")
  ] in
  let case = ECase(variant, handlers) in
  assert_ty case TyUnit;
  let extra_handlers = StrMap.add "c"  EUnit handlers in
  let case_with_extra = ECase(variant, extra_handlers) in
  begin match type_of case_with_extra with
    | exception (Expected_Variant_Type_W_Ctors _ ) -> ()
    | _ ->
      failwith "expected exception"
  end;
  let bad_handlers = StrMap.add "b" ETrue handlers in
  check_fails_ty_mismatch (ECase(variant, bad_handlers))
