(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open Cmdliner
open OpamTypes
open OpamTypesBase
open OpamStd.Op

let main_catch_all f =
  try f () with
  | OpamStd.Sys.Exit 0 -> ()
  | OpamStd.Sys.Exec (cmd,args,env) ->
    OpamStd.Sys.exec_at_exit ();
    Unix.execvpe cmd args env
  | e ->
    flush stdout;
    flush stderr;
    if (OpamConsole.verbose ()) then
      OpamConsole.errmsg "'%s' failed.\n"
        (String.concat " " (Array.to_list Sys.argv));
    let exit_code = match e with
      | OpamStd.Sys.Exit i ->
        if (OpamConsole.debug ()) && i <> 0 then
          OpamConsole.errmsg "%s" (OpamStd.Exn.pretty_backtrace e);
        i
      | OpamSystem.Internal_error _ ->
        OpamConsole.errmsg "%s\n" (Printexc.to_string e);
        OpamStd.Sys.get_exit_code `Internal_error
      | OpamSystem.Process_error result ->
        OpamConsole.errmsg "%s Command %S failed:\n%s\n"
          (OpamConsole.colorise `red "[ERROR]")
          (try List.assoc "command" result.OpamProcess.r_info with
           | Not_found -> "")
          (Printexc.to_string e);
        OpamConsole.errmsg "%s" (OpamStd.Exn.pretty_backtrace e);
        OpamStd.Sys.get_exit_code `Internal_error
      | Sys.Break
      | OpamParallel.Errors (_, (_, Sys.Break)::_, _) ->
        OpamStd.Sys.get_exit_code `User_interrupt
      | Sys_error e when e = "Broken pipe" ->
        (* workaround warning 52, this is a fallback (we already handle the
           signal) and there is no way around at the moment *)
        141
      | Failure msg ->
        OpamConsole.errmsg "Fatal error: %s\n" msg;
        OpamConsole.errmsg "%s" (OpamStd.Exn.pretty_backtrace e);
        OpamStd.Sys.get_exit_code `Internal_error
      | _ ->
        OpamConsole.errmsg "Fatal error:\n%s\n" (Printexc.to_string e);
        OpamConsole.errmsg "%s" (OpamStd.Exn.pretty_backtrace e);
        OpamStd.Sys.get_exit_code `Internal_error
    in
    exit exit_code

let run command =
  OpamStd.Option.iter OpamVersion.set_git OpamGitVersion.version;
  OpamSystem.init ();
  main_catch_all @@ fun () ->
  let eval () =
    Term.eval ~catch:false ~argv:Sys.argv command
  in
  match eval () with
  | `Error _ -> exit (OpamStd.Sys.get_exit_code `Bad_arguments)
  | _        -> exit (OpamStd.Sys.get_exit_code `Success)

type nix_dep = {
  is_required : bool ;
  ever_required : bool ;
  filtered_constraints : (OpamTypes.filter list * (relop * string) OpamFormula.formula) list ;
  include_conditions : OpamTypes.filter ;
}

module NixDeps = struct
  open OpamPackage

  type t = {
    ordering : Name.t list ;
    details : nix_dep Name.Map.t ;
  }

  let bindings { ordering ; details } = ordering |> List.map (fun p -> (p, Name.Map.find p details))
  let empty = { ordering = [] ; details = Name.Map.empty }
  let iter f { ordering ; details } = ordering |> List.iter (fun p -> f p @@ Name.Map.find p details)
  let keys { ordering ; _ } = ordering
  let mem k { details ; _ } = Name.Map.mem k details
  let singleton p v = { ordering = [p] ; details = Name.Map.singleton p v }
  let union x y = {
    ordering = x.ordering @ List.filter (fun p -> not (Name.Map.mem p x.details)) y.ordering ;
    details = OpamPackage.Name.Map.union
      (fun d1 d2 ->
        { is_required = d1.is_required || d2.is_required ;
          ever_required = d1.ever_required || d2.ever_required ;
          filtered_constraints = d1.filtered_constraints @ d2.filtered_constraints ;
          include_conditions = FOr (d1.include_conditions,d2.include_conditions) })
      x.details
      y.details
  }

end

(*
module NixTypes = struct
  type 'x any = [`NAp of ('x * 'x) | `NVar of string | `NAttr of ('x * string)]
  type 'x nbool = ['x any | `NTrue | `NFalse | `NAnd of ('x * 'x) | `NOr of ('x * 'x) | `NNot of 'x | `NImpl of ('x * 'x) | `NEq of ('x * 'x) | `NNeq of ('x * 'x)]
  type nconst = [`NTrue | `NFalse | `NVer of string | `NStr of string]
  type 'x nvar = [`NAnd of ('x * 'x) | `NOr of ('x * 'x) | `NNot of 'x | `NImpl of ('x * 'x) | `NEq of ('x * 'x) | `NNeq of ('x * 'x)]
  type 'x fn = 'x any
  type 'x expr = [nconst | `NAp of ([expr & expr fn]) | `NVar of string | `NAttr of ([expr obj & expr] * string)]
end

type vnix_bool = [`NAnd of (vnix_bool * vnix_bool) | `NOr of (vnix_bool * vnix_bool) | `NNot of vnix_bool | `NImpl of (vnix_bool * vnix_bool) | `NVar of string]

type nix_bool = [`NTrue | `NFalse | vnix_bool]
*)

type nix_type = [`NTBool | `NTStr | `NTList of nix_type | `NTSet | `NTPath | `NTFun ]
type nix_expr = [`NTrue | `NFalse | `NAnd of (nix_expr * nix_expr) | `NOr of (nix_expr * nix_expr) | `NNot of nix_expr | `NImpl of (nix_expr * nix_expr) | `NVar of string | `NNull | `NAp of (nix_expr * nix_expr) | `NAttr of (nix_expr * string) | `NEq of (nix_expr * nix_expr) | `NNeq of (nix_expr * nix_expr) | `NList of nix_expr list | `NStr of string | `NStrI of [`NLit of string | `NInterp of nix_expr] list | `NSet of (string * nix_expr) list | `NIf of (nix_expr * nix_expr * nix_expr) | `NPath of string | `NAppend of (nix_expr * nix_expr) | `NTy of (nix_type * nix_expr)]
type nix_const = [`NTrue | `NFalse | `NStr of string | `NNull]

type nix_pkg = {
  pname: OpamPackage.Name.t;
  version: OpamPackage.Version.t;
  deps: NixDeps.t;
  prop_deps: NixDeps.t;
  conflicts: NixDeps.t;
  build: nix_expr;
  install: nix_expr;
  patches: nix_expr;
  src: nix_expr;
  extra_src: (OpamTypes.basename * nix_expr) list;
  uses_zip: bool;
}

exception Unsupported of string
exception Wat of string
exception Waat

let nix_true = `NTrue
let nix_false = `NFalse
let nix_not b = match b with
  | `NTrue -> `NFalse
  | `NFalse -> `NTrue
  | `NNot v -> v
  | `NEq (l,r) -> `NNeq (l,r)
  | `NNeq (l,r) -> `NEq (l,r)
  | v -> `NNot v
let nix_and l r = match l, r with
  | `NTrue, _ -> r
  | `NFalse, _ -> l
  | _, `NTrue -> l
  | _, `NFalse -> r
  | x , y -> `NAnd (x,y)
let nix_or l r = match l, r with
  | `NTrue, _ -> l
  | `NFalse, _ -> r
  | _, `NTrue -> r
  | _, `NFalse -> l
  | x , y -> `NOr (x,y)
let nix_impl l r = match l, r with
  | `NTrue, _ -> r
  | `NFalse, _ -> l
  | _, `NTrue -> r
  | _, `NFalse -> nix_not l
  | x , y -> `NImpl (x,y)
let nix_if c t e = match c with
  | `NTrue -> t
  | `NFalse -> e
  | _ -> `NIf (c,t,e)
let nix_var (v : string) = `NVar v
let nix_null = `NNull
let nix_str (s : string) = `NStr s
let nix_eq l r = match l, r with
  | `NTrue, `NTrue -> `NTrue
  | `NFalse, `NFalse -> `NTrue
  | `NStr x, `NStr y -> if x = y then `NTrue else `NFalse
  | #nix_const, #nix_const -> `NFalse
  | _, _ -> `NEq (l,r)
let nix_neq l r = nix_not (nix_eq l r)
let nix_list vs = `NList vs
let nix_is_bool x = match x with
  | `NTrue | `NFalse | `NNot _ | `NAnd (_,_) | `NOr (_,_) | `NImpl (_,_) | `NEq (_,_) | `NNeq (_,_) | `NTy (`NTBool,_) -> `NTrue
  | `NStr _ | `NStrI _ | `NNull | `NSet _ | `NList _ | `NPath _ | `NTy (_,_) -> `NFalse
  | _ -> `NAp (`NAttr (`NVar "builtin","isBool"),x)
let nix_stringify x = nix_if (nix_is_bool x) (nix_if x (nix_str "true") (nix_str "false")) x
let nix_stri ls = match ls with
  | [] -> nix_str ""
  | `NLit l :: [] -> nix_str l
  | _ -> `NStrI ls
let rec nix_strc ls r = match ls, r with
  | ls, `NStr "" -> ls
  | `NLit s :: [], `NStr r -> [`NLit (s ^ r)]
  | `NLit s :: [], `NStrI (`NLit r :: rs) -> `NLit (s ^ r) :: rs
  | `NLit "" :: [], `NStrI rs -> rs
  | `NLit s :: [], `NStrI rs -> `NLit s :: rs
  | `NLit "" :: [], r -> [`NInterp r]
  | x :: [], `NStr r -> [x; `NLit r]
  | x :: [], `NStrI rs -> x :: rs
  | x :: [], r -> [x; `NInterp r]
  | l :: ls, r -> l :: nix_strc ls r
  | _ -> raise Waat
let nix_str_append l r = match nix_stringify l, nix_stringify r with
  | `NStrI ss, r -> nix_stri @@ nix_strc ss r
  | `NStr s, r -> nix_stri @@ nix_strc [`NLit s] r
  | l, r -> nix_stri @@ nix_strc [`NInterp l] r
let nix_path s = match String.index_opt s '/' with
  | None -> `NPath ("./" ^ s)
  | Some _ -> `NPath s
let nix_ap f x = `NAp (f,x)
let nix_attr x a = `NAttr (x,a)
let nix_stdlib = nix_attr @@ nix_attr (nix_var "stdenv") "lib"
let nix_stdcall f = nix_ap @@ nix_stdlib f
let nix_stdcall2 f x = nix_ap @@ nix_stdcall f x
let nix_typed t x = `NTy (t,x)
let rec nix_append_lists = function
  | `NList x as nx -> (function
    | `NList y -> nix_list (x @ y)
    | `NAppend (`NList y,z) -> `NAppend (nix_list (x @ y), z)
    | y -> `NAppend (nx, y))
  | `NAppend (x,y) -> fun z -> `NAppend (x, nix_append_lists y z)
  | x -> fun y -> `NAppend (x,y)
let rec nix_concat_lists_ = function
  | l :: ls -> nix_append_lists l (nix_concat_lists_ ls)
  | [] -> nix_list []
let rec nix_concat_lists = function
  | `NList ls -> nix_concat_lists_ ls
  | `NAppend (x,y) -> nix_append_lists (nix_concat_lists x) (nix_concat_lists y)
  | x -> nix_stdcall "concatLists" x

let nix_escape s = Re.replace (Re.compile (Re.alt [Re.char '\\'; Re.char '"'; Re.str "${"; Re.str "\n"])) ~f:(fun g -> "\\" ^ (fun c -> if c = "\n" then "n" else c) (Re.Group.get g 0)) s
let shell_escape s = "'" ^ Re.replace (Re.compile (Re.char '\'')) ~f:(fun _ -> "'\\''") s ^ "'"

let argname_of_pkgname p =
  match OpamPackage.Name.to_string p with
  | "ocamlfind" -> "findlib"
  | n -> n

let nix_ver_of_pkg pkg = `NAp (`NAttr (`NAttr(`NVar "stdenv","lib"),"getVersion"),`NVar (argname_of_pkgname pkg))
let nix_ver_of_filter flt = `NStr flt

let optionality_env v = match OpamVariable.Full.scope v, OpamVariable.to_string (OpamVariable.Full.variable v) with
  | Global,"build" -> Some (B true)
  | Global,"os" -> Some (S "linux")
  | Global,"os-distribution" -> Some (S "nixos")
  | Global,"with-doc" -> Some (B true)
  | _,_ -> None

let propagation_env v = match OpamVariable.Full.scope v, OpamVariable.to_string (OpamVariable.Full.variable v) with
  | Global,"build" -> Some (B false)
  | _,_ -> None

let rec resolve_ident = function
  | (_::_::_) as ps,v -> List.fold_left nix_and nix_true @@ List.map (fun p -> resolve_ident ([p], v)) ps
  | [],"name" -> nix_typed `NTStr @@ nix_var "pname"
  | [],"jobs" -> nix_str "1"
  | [],"make" -> nix_str "make"
  | [],"lib" -> nix_str "$OCAMLFIND_DESTDIR"
  | [],"bin" -> nix_str "$out/bin"
  | [],"man" -> nix_str "$out/man"
  | [],"doc" -> nix_str "$out/share/doc"
  | [],"prefix" -> nix_str "$out"
  | [],"pinned" -> nix_false
  | [],"build" -> nix_true
  | [],"post" -> raise @@ Unsupported "post dependency"
  | [],"with-test" -> nix_typed `NTBool @@ nix_var "doCheck"
  | [],"with-doc" -> nix_true
  | [],"os" -> nix_str "linux"
  | [],"os-distribution" -> nix_str "nixos"
  | ["ocaml"],"native" -> nix_not (`NAttr (nix_var "stdenv","isMips"))
  | ["ocaml"],"native-dynlink" -> nix_not (`NAttr (nix_var "stdenv","isMips"))
  | ["ocaml"],"preinstalled" -> nix_true
  | [p],"installed" -> nix_neq (nix_var p) (nix_var "null")
  | ps,v -> raise @@ Wat (OpamFilter.to_string @@ FIdent (List.map (function | "_" -> None | p -> Some (OpamPackage.Name.of_string p)) ps, OpamVariable.of_string v, None))

let rec nix_bool_of_filter flt = match flt with
  | FBool true -> nix_true
  | FBool false -> nix_false
  | FAnd (l,r) -> nix_and (nix_bool_of_filter l) (nix_bool_of_filter r)
  | FOr (l,r) -> nix_or (nix_bool_of_filter l) (nix_bool_of_filter r)
  | FIdent (ps,v,None) -> resolve_ident (List.map (OpamStd.Option.map_default OpamPackage.Name.to_string "_") ps, OpamVariable.to_string v)
  | FOp (l,`Eq,r) -> nix_eq (nix_bool_of_filter l) (nix_bool_of_filter r)
  | FOp (l,`Neq,r) -> nix_neq (nix_bool_of_filter l) (nix_bool_of_filter r)
  | FString s -> nix_str s
  | FNot x -> nix_not (nix_bool_of_filter x)
  | f -> raise @@ Wat (OpamFilter.to_string f)

let nix_bool_of_formula nix_bool_of_atom =
  let rec go nix_bool_of_empty formula = match formula with
  | Empty -> nix_bool_of_empty
  | Atom x -> nix_bool_of_atom x
  | Block x -> go nix_bool_of_empty x
  | And (x,y) -> nix_and (go nix_true x) (go nix_true y)
  | Or (x,y) -> nix_or (go nix_false x) (go nix_false y)
  in
  go

let nix_ver_cmp op v1 v2 =
  `NAp (`NAp (`NAttr (`NAttr ((`NVar "stdenv"), "lib"), op), v1), v2)

let nix_optionals_opt b l = match b with
  | `NTrue -> Some l
  | `NFalse -> None
  | cond -> match l with
    | `NList [] -> None
    | `NList (x::[]) -> Some (nix_stdcall2 "optional" cond x)
    | _ -> Some (nix_stdcall2 "optionals" cond l)

let nix_optionals b l = match nix_optionals_opt b l with
  | Some l -> l
  | None -> nix_list []

let nix_bool_of_constraint pkg (relop, ver) =
  let chk_ver = nix_ver_of_filter ver in
  let pkg_ver = nix_ver_of_pkg pkg in
  match relop with
  | `Eq -> nix_eq pkg_ver chk_ver
  | `Neq -> nix_neq pkg_ver chk_ver
  | `Geq -> nix_ver_cmp "versionAtLeast" pkg_ver chk_ver
  | `Gt -> nix_ver_cmp "versionOlder" chk_ver pkg_ver
  | `Leq -> nix_ver_cmp "versionAtLeast" chk_ver pkg_ver
  | `Lt -> nix_ver_cmp "versionOlder" pkg_ver chk_ver

let rec pp_nix_expr_prec prec ppf nb =
  let open Format in
  match nb with
  | `NAnd (l,r) ->
      let paren = match prec with
        | `PElse | `PImpl | `POr | `PAnd -> false
        | _ -> true
      in
      if paren then pp_print_text ppf "(" else ();
      pp_nix_expr_prec `PAnd ppf l;
      pp_print_text ppf " && ";
      pp_nix_expr_prec `PAnd ppf r;
      if paren then pp_print_text ppf ")" else ()
  | `NOr (l,r) ->
      let paren = match prec with
        | `PElse | `PImpl | `POr -> false
        | _ -> true
      in
      if paren then pp_print_text ppf "(" else ();
      pp_nix_expr_prec `POr ppf l;
      pp_print_text ppf " || ";
      pp_nix_expr_prec `POr ppf r;
      if paren then pp_print_text ppf ")" else ()
  | `NImpl (l,r) ->
      let paren = match prec with
        | `PElse -> false
        | _ -> true
      in
      if paren then pp_print_text ppf "(" else ();
      pp_nix_expr_prec `PImpl ppf l;
      pp_print_text ppf " -> ";
      pp_nix_expr_prec `PImpl ppf r;
      if paren then pp_print_text ppf ")" else ()
  | `NNot v ->
      let paren = match prec with
        | `PElse | `PImpl | `POr | `PAnd | `PEq -> false
        | _ -> true
      in
      if paren then pp_print_text ppf "(" else ();
      pp_print_text ppf "!";
      pp_nix_expr_prec `PNot ppf v;
      if paren then pp_print_text ppf ")" else ()
  | `NTrue -> pp_print_text ppf "true"
  | `NFalse -> pp_print_text ppf "false"
  | `NVar v ->
      pp_print_text ppf v
  | `NNull -> pp_print_text ppf "null"
  | `NAp (f, x) ->
      let paren = match prec with
        | `PElse | `PImpl | `POr | `PAnd | `PEq | `PNot | `PAppend -> false
        | _ -> true
      in
      if paren then pp_print_text ppf "(" else ();
      pp_nix_expr_prec `PNot ppf f;
      pp_print_text ppf " ";
      pp_nix_expr_prec `PAp ppf x;
      if paren then pp_print_text ppf ")" else ()
  | `NEq (l,r) ->
      let paren = match prec with
        | `PElse | `PImpl | `POr | `PAnd -> false
        | _ -> true
      in
      if paren then pp_print_text ppf "(" else ();
      pp_nix_expr_prec `PEq ppf l;
      pp_print_text ppf " == ";
      pp_nix_expr_prec `PEq ppf r;
      if paren then pp_print_text ppf ")" else ()
  | `NNeq (l,r) ->
      let paren = match prec with
        | `PElse | `PImpl | `POr | `PAnd -> false
        | _ -> true
      in
      if paren then pp_print_text ppf "(" else ();
      pp_nix_expr_prec `PEq ppf l;
      pp_print_text ppf " != ";
      pp_nix_expr_prec `PEq ppf r;
      if paren then pp_print_text ppf ")" else ()
  | `NAttr (l,a) ->
      let paren = match prec with
        | `PElse | `PImpl | `POr | `PAnd | `PEq | `PNot | `PAppend | `PAp | `PAttr -> false
        | _ -> true
      in
      if paren then pp_print_text ppf "(" else ();
      pp_nix_expr_prec `PAttr ppf l;
      pp_print_text ppf ".";
      pp_print_text ppf a;
      if paren then pp_print_text ppf ")" else ()
  | `NAppend (x,y) ->
      let paren = match prec with
        | `PElse | `PImpl | `POr | `PAnd | `PEq | `PNot | `PAppend -> false
        | _ -> true
      in
      if paren then pp_print_text ppf "(" else ();
      pp_nix_expr_prec `PAppend ppf x;
      pp_print_text ppf " ++ ";
      pp_nix_expr_prec `PAppend ppf y;
      if paren then pp_print_text ppf ")" else ()
  | `NStr s ->
      fprintf ppf "@[<h>\"%s\"@]" @@ nix_escape s
  | `NStrI pieces -> fprintf ppf "@[<h>\"%a\"@]" pp_nix_str pieces
  | `NList pieces ->
      fprintf ppf "[@;<1 2>@[";
      pieces |> List.iter (fun x ->
        pp_nix_expr_prec `PList ppf x;
        fprintf ppf "@ ");
      fprintf ppf "@]]"
  | `NSet attributes ->
      fprintf ppf "{@;<1 2>@[<hv>%a@]@ }"
        (pp_print_list ~pp_sep:pp_print_space pp_nix_attribute)
        attributes
  | `NIf (cond,tbranch,ebranch) ->
      let paren = match prec with
        | _ -> false
      in
      if paren then pp_print_text ppf "(" else ();
      fprintf ppf "@[if@ @[";
      pp_nix_expr_prec `PElse ppf cond;
      fprintf ppf "@]@ @[@,then@[@ ";
      pp_nix_expr_prec `PElse ppf tbranch;
      fprintf ppf "@]@ else@[@ ";
      pp_nix_expr_prec `PElse ppf ebranch;
      fprintf ppf "@,@]@,@]";
      if paren then pp_print_text ppf ")" else ()
  | `NPath p ->
      fprintf ppf "%s" p
  | `NTy (_t,e) ->
      pp_nix_expr_prec prec ppf e
and pp_nix_str ppf pieces = pieces |> List.iter
  (fun piece -> let open Format in
     match piece with
     | `NLit s -> pp_print_text ppf @@ nix_escape s
     | `NInterp exp ->
       pp_print_text ppf "${";
       pp_nix_expr_prec `PElse ppf exp;
       pp_print_text ppf "}")
and pp_nix_attribute ppf (k,v) =
  Format.fprintf ppf "@[%s = %a;@]" k (pp_nix_expr_prec `PElse) v

let pp_nix_expr = pp_nix_expr_prec `PElse

let nix_arg name deflt = `NArg (name, deflt)

let arg_of_dep (p, {is_required; _}) =
  nix_arg (argname_of_pkgname p) @@ if is_required then None else Some nix_null

let pp_nix_arg ppf = function
  | `NArg (name, None) -> Format.fprintf ppf "%s" name
  | `NArg (name, Some x) -> Format.fprintf ppf "%s ? %a" name pp_nix_expr x

let pp_nix_args ppf args =
  let open Format in
  fprintf ppf "@[<2>{@ ";
  pp_print_list ~pp_sep:(fun ppf _ -> fprintf ppf ",@ ") pp_nix_arg ppf args;
  fprintf ppf "@ }:@]@ "

let pp_nix_pkg ?opam ppf nix_pkg =
  let open Format in
  fprintf ppf "@[<hv>";
  (match opam with
  | None -> ();
  | Some file ->
      fprintf ppf "/*@[";
      pp_print_text ppf @@ OpamFile.OPAM.write_to_string file;
      fprintf ppf "@]*/@;");
  pp_nix_args ppf ((if nix_pkg.uses_zip then [nix_arg "unzip" None] else []) @ [nix_arg "doCheck" @@ Some nix_false; nix_arg "stdenv" None; nix_arg "opam" None; nix_arg "fetchurl" None] @ List.map arg_of_dep (NixDeps.bindings nix_pkg.deps)) ;
  nix_pkg.deps |> NixDeps.iter (fun name { ever_required; filtered_constraints; _ } ->
    filtered_constraints |> List.iter (fun (filters, constraints) ->
      let guard = List.fold_left (fun l r -> nix_and l @@ nix_bool_of_filter r) (if ever_required then nix_true else nix_neq (nix_var @@ argname_of_pkgname name) nix_null) filters in
      let cond = nix_bool_of_formula (nix_bool_of_constraint name) `NTrue constraints in
      let asserted = nix_impl guard cond in
      match asserted with
      | `NTrue -> ()
      | _ -> fprintf ppf "@[<2>assert %a;@]@ " pp_nix_expr asserted));
  nix_pkg.conflicts |> NixDeps.iter (fun name { filtered_constraints; _ } ->
    if NixDeps.mem name nix_pkg.deps then
      filtered_constraints |> List.iter (fun (filters, constraints) ->
        let guard = List.fold_left (fun l r -> nix_and l @@ nix_bool_of_filter r) `NTrue filters in
        let cond = nix_bool_of_formula (nix_bool_of_constraint name) `NTrue constraints in
        let asserted = nix_impl guard (nix_not cond) in
        match asserted with
        | `NTrue -> ()
        | _ -> fprintf ppf "@[<2>assert %a;@]@ " pp_nix_expr asserted)
    else ());
  fprintf ppf "@ stdenv.mkDerivation rec {@;<1 2>@[<hv>";
  fprintf ppf "pname = ";
  pp_nix_expr ppf (nix_str (OpamPackage.Name.to_string nix_pkg.pname));
  fprintf ppf ";@ ";
  fprintf ppf "version = ";
  pp_nix_expr ppf (nix_str (OpamPackage.Version.to_string nix_pkg.version));
  fprintf ppf ";@ ";
  fprintf ppf "name = \"${pname}-${version}\"";
  fprintf ppf ";@ ";
  fprintf ppf "inherit doCheck";
  fprintf ppf ";@ ";
  fprintf ppf "src = ";
  pp_nix_expr ppf nix_pkg.src;
  (match List.map (fun (name,src) -> nix_str_append (nix_str "ln -sv ") (nix_str_append src (nix_str @@ " \"$sourceRoot\"/" ^ OpamFilename.Base.to_string name))) nix_pkg.extra_src with
  | [] -> ()
  | x::xs ->
      fprintf ppf ";@ ";
      fprintf ppf "postUnpack = ";
      pp_nix_expr ppf @@ List.fold_left (fun x y -> nix_str_append x @@ nix_str_append (nix_str "\n") y) x xs);
  fprintf ppf ";@ ";
  let buildInputs = NixDeps.bindings nix_pkg.deps
    |> OpamStd.List.filter_map (fun (p, { include_conditions ; _ }) ->
        nix_optionals_opt
          (nix_bool_of_filter include_conditions)
          (nix_list [nix_var @@ argname_of_pkgname p]))
    |> nix_list
    |> nix_concat_lists
  in
  fprintf ppf "buildInputs = ";
  pp_nix_expr ppf @@ nix_append_lists (nix_list @@ if nix_pkg.uses_zip then [nix_var "unzip"] else []) buildInputs;
  fprintf ppf ";@ ";
  let propagatedBuildInputs = NixDeps.bindings nix_pkg.prop_deps
    |> OpamStd.List.filter_map (fun (p, { include_conditions ; _ }) ->
        nix_optionals_opt
          (nix_bool_of_filter @@ OpamFilter.partial_eval propagation_env include_conditions)
          (nix_list [nix_var @@ argname_of_pkgname p]))
    |> nix_list
    |> nix_concat_lists
  in
  fprintf ppf "propagatedBuildInputs = ";
  pp_nix_expr ppf propagatedBuildInputs;
  fprintf ppf ";@ ";
  fprintf ppf "configurePhase = \"true\"";
  fprintf ppf ";@ ";
  (match nix_pkg.patches with
  | `NList [] -> ()
  | _ ->
    fprintf ppf "patches = ";
    pp_nix_expr ppf nix_pkg.patches;
    fprintf ppf ";@ "
  );
  fprintf ppf "buildPhase = stdenv.lib.concatMapStringsSep \"\\n\" (stdenv.lib.concatStringsSep \" \")@ ";
  pp_nix_expr ppf nix_pkg.build;
  fprintf ppf ";@ ";
  fprintf ppf "preInstall = stdenv.lib.concatMapStringsSep \"\\n\" (stdenv.lib.concatStringsSep \" \")@ ";
  pp_nix_expr ppf nix_pkg.install;
  fprintf ppf ";@ ";
  fprintf ppf "installPhase = \"runHook preInstall; mkdir -p $out; for i in *.install; do ${opam.installer}/bin/opam-installer -i --prefix=$out --libdir=$OCAMLFIND_DESTDIR \\\"$i\\\"; done\"";
  fprintf ppf ";@ ";
  fprintf ppf "createFindlibDestdir = true";
  fprintf ppf ";";
  fprintf ppf "@]@ }";
  fprintf ppf "@]";
  fprintf ppf "@.";
  ()

let nixdep_of_filtered_constraints fc =
  let rec separate fc = match fc with
  | Empty -> [], Empty
  | Block _ -> raise Waat
  | And (l, r) ->
      let lp, la = separate l in
      let rp, ra = separate r in
      lp @ rp, And (la, ra)
  | Or (l, r) -> (
      let lp, la = separate l in
      let rp, ra = separate r in
      match lp, rp with
      | [], [] -> [], Or (la, ra)
      | _, _ -> raise Waat)
  | Atom (Filter f) -> [f], Empty
  | Atom (Constraint (_relop, FBool _)) -> raise Waat
  | Atom (Constraint (relop, FString ver)) -> [], Atom (relop, ver)
  | Atom (Constraint (_relop, FIdent (_,_,_))) -> raise Waat
  | Atom (Constraint (_relop, FOp (_,_,_))) -> raise Waat
  | Atom (Constraint (_relop, FAnd (_,_))) -> raise Waat
  | Atom (Constraint (_relop, FOr (_,_))) -> raise Waat
  | Atom (Constraint (_relop, FNot _)) -> raise Waat
  | Atom (Constraint (_relop, FDefined _)) -> raise Waat
  | Atom (Constraint (_relop, FUndef _)) -> raise Waat
  in
  let rec cond fc = match fc with
  | Empty -> FBool true
  | Block _ -> raise Waat
  | And (l, r) -> FOr (cond l, cond r)
  | Or (l, r) -> (match cond l, cond r with
    | FBool true, FBool true -> FBool true
    | _, _ -> raise Waat)
  | Atom (Filter f) -> f
  | Atom (Constraint _) -> FBool true
  in
  let filtered_constraints = [separate fc] in
  let include_conditions = cond fc in
  let optionality_fc = OpamFormula.partial_eval (function
    | Filter f -> (match OpamFilter.partial_eval optionality_env f with
      | FBool true -> `True
      | FBool false -> `False
      | r -> `Formula (Atom (Filter r)))
    | Constraint (_,_) -> `True) fc in
  let is_required = match optionality_fc with
  | `True -> true
  | `False -> false
  | `Formula Empty -> true
  | `Formula f -> false
  in
  { is_required; ever_required = (optionality_fc <> `False); filtered_constraints; include_conditions }

exception Unclosed_variable_replacement of string

let string_interp_regex =
  let open Re in
  let notclose =
    rep (alt [
        diff notnl (set "}");
        seq [char '}'; alt [diff notnl (set "%"); stop] ]
      ])
  in
  compile (alt [
      str "%%";
      seq [str "%{"; group (greedy notclose); opt (group (str "}%"))];
    ])

let parse_ident s =
  match OpamStd.String.rcut_at s ':' with
  | None -> [], s
  | Some (p,last) ->
      match OpamStd.String.rcut_at p '?' with
      | None ->
          OpamStd.String.split p '+', last
      | Some (_p,_) ->
          raise @@ Wat s

let nix_expand_string s = let f = function
  | `Delim g -> (let str = Re.Group.get g 0 in
    if str = "%%" then nix_str "%"
    else if not (OpamStd.String.ends_with ~suffix:"}%" str) then
      raise @@ Unclosed_variable_replacement str
    else resolve_ident @@ parse_ident (String.sub str 2 (String.length str - 4)))
  | `Text t -> nix_str (shell_escape t)
  in
  List.fold_right (fun x -> nix_str_append (f x)) (Re.split_full string_interp_regex s) (nix_str "")

let nix_expr_of_arg = function
  | CString s -> nix_list [nix_expand_string s]
  | CIdent v -> nix_list [resolve_ident @@ parse_ident v]

let nix_expr_of_patch p = nix_list [nix_str @@ OpamFilename.Base.to_string p]

let nix_expr_of_patches args = `NList (
  args |> OpamStd.List.filter_map (fun (arg, flt) ->
    nix_optionals_opt
      (OpamStd.Option.map_default nix_bool_of_filter `NTrue flt)
      (nix_expr_of_patch arg)))
    |> nix_concat_lists

let nix_expr_of_args args = `NList (
  args |> OpamStd.List.filter_map (fun (arg, flt) ->
    nix_optionals_opt
      (OpamStd.Option.map_default nix_bool_of_filter `NTrue flt)
      (nix_expr_of_arg arg)))
    |> nix_concat_lists

let nix_expr_of_commands cmds = `NList (
  cmds |> OpamStd.List.filter_map (fun (args, flt) ->
    nix_optionals_opt
      (OpamStd.Option.map_default nix_bool_of_filter `NTrue flt)
      (nix_expr_of_args args)))

let nix_name ~refnames name =
  OpamStd.Option.default name (OpamPackage.Name.Map.find_opt name refnames)

let rec nixdeps_of_depends ~refnames depends = match depends with
  | Empty -> NixDeps.empty
  | Block _ -> raise Waat
  | And (l, r) -> NixDeps.union
    (nixdeps_of_depends ~refnames l)
    (nixdeps_of_depends ~refnames r)
  | Or (l, r) -> raise @@ Wat (OpamFilter.string_of_filtered_formula l ^ " OROROR " ^ OpamFilter.string_of_filtered_formula r)
  | Atom (name, cs) -> NixDeps.singleton (nix_name ~refnames name) @@ nixdep_of_filtered_constraints cs

let rec nixdeps_of_depopts ~refnames depopts = match depopts with
  | Empty -> NixDeps.empty
  | Block _ -> raise Waat
  | And (l, r) -> raise @@ Wat (OpamFilter.string_of_filtered_formula l ^ " ANDANDAND " ^ OpamFilter.string_of_filtered_formula r)
  | Or (l, r) -> NixDeps.union (nixdeps_of_depopts ~refnames l) (nixdeps_of_depopts ~refnames r)
  | Atom (name, cs) -> NixDeps.singleton (nix_name ~refnames name) @@
    { (nixdep_of_filtered_constraints cs) with is_required = false; ever_required = false; include_conditions = FIdent([Some name],OpamVariable.of_string "installed",None) }

let nixdeps_of_depexts depexts =
  depexts |> List.map (fun (pkgs, flt) ->
    nix_optionals_opt
      (nix_bool_of_filter flt)
      (nix_list @@ List.map nix_str pkgs)
    |> function
      | None -> []
      | Some (`NList xs) -> xs |> List.map (function | `NStr name -> NixDeps.singleton (OpamPackage.Name.of_string name) { is_required = true; ever_required = true; filtered_constraints = []; include_conditions = FBool true }
                                                     | _ -> raise @@ Wat "depext constraint too complex")
      | Some _ -> raise @@ Wat "depext constraint too complex")
  |> List.concat
  |> NixDeps.(List.fold_left union empty)

(* TODO: this *)
let active_caches _st _nv = []

let is_zip_archive f =
  try
    let ic = open_in f in
    let c1 = input_char ic in
    let c2 = input_char ic in
    let c3 = input_char ic in
    let c4 = input_char ic in
    close_in ic;
    match c1, c2, c3, c4 with
    | '\x50', '\x4b', '\x03', '\x04' -> true
    | _ -> false
  with Sys_error _ | End_of_file -> false


let nix_src_of_opam name version opam =
  OpamGlobalState.with_ `Lock_write @@ fun gt ->
  OpamSwitchState.with_ `Lock_write gt @@ fun st ->
  let open OpamProcess.Job.Op in
  let nv = OpamPackage.create name version in
  let cache_dir = OpamRepositoryPath.download_cache st.switch_global.root in
  let cache_urls = active_caches st nv in

  let fetch_source_job =
    match OpamFile.OPAM.url opam with
    | None   -> Done (Ok None)
    | Some u ->
      OpamRepository.pull_file_to_cache (OpamPackage.to_string nv)
        ~cache_dir ~cache_urls
        (OpamFile.URL.checksum u)
        (OpamFile.URL.url u :: OpamFile.URL.mirrors u)
      @@| function
        | Not_available (_,na) -> Error na
        | _ -> Ok (Some (OpamFile.URL.url u, None, OpamRepository.cache_file cache_dir (List.hd (OpamFile.URL.checksum u))))

  in
  let fetch_extra_source_job (name, u) =
    OpamRepository.pull_file_to_cache
      (OpamPackage.to_string nv ^"/"^ OpamFilename.Base.to_string name)
      ~cache_dir ~cache_urls
      (OpamFile.URL.checksum u)
      (OpamFile.URL.url u :: OpamFile.URL.mirrors u)
    @@| function
      | Not_available (_,na) -> Error na
      | _ -> Ok (Some (OpamFile.URL.url u, Some name, OpamRepository.cache_file cache_dir (List.hd (OpamFile.URL.checksum u))))
  in
  let jobs = fetch_source_job :: List.map fetch_extra_source_job (OpamFile.OPAM.extra_sources opam) in
  let jobs = jobs |> List.map (fun dl ->
    function
      | Error e -> Done (Error e)
      | Ok rs -> dl @@+ (function
        | Error e -> Done (Error e)
        | Ok None -> Done (Ok rs)
        | Ok (Some (url, name, file)) ->
          OpamSystem.make_command "nix-hash" ["--type"; "sha256"; "--base32"; "--flat"; OpamFilename.to_string file] @@> (fun result ->
            OpamProcess.cleanup ~force:true result;
            if OpamProcess.is_success result then
              let is_zip = is_zip_archive (OpamFilename.to_string file) in
              let fetch = `NAp (nix_var "fetchurl", `NSet ["url", `NStr (OpamUrl.to_string url); "sha256", `NStr (List.hd @@ result.r_stdout)]) in
              match name with
              | None -> Done (Ok (fun (_, xs, uses_zip) -> rs @@ (fetch, xs, uses_zip || is_zip)))
              | Some name -> Done (Ok (fun (src, xs, uses_zip) -> rs @@ (src, (name, fetch) :: xs, uses_zip || is_zip)))
            else
              Done (Error "SHA256 computation failed"))))
  in
  let blankSrc = `NStr "/var/empty" in
  let r = OpamProcess.Job.run @@ OpamProcess.Job.seq jobs (Ok (fun xs -> xs))
  in match r with
    | Error e -> raise @@ Wat e
    | Ok srcs -> srcs (blankSrc, [], false)

let nix_of_opam ?name ?version ~refnames ~patches ~extra_depexts opam =
  let name = match name with
  | Some x -> x
  | None -> OpamFile.OPAM.name opam in
  let version = match version with
  | Some x -> x
  | None -> OpamFile.OPAM.version opam in
  let _ref_name = nix_name ~refnames name in
  let deps = nixdeps_of_depends ~refnames (And (opam.depends, Atom (OpamPackage.Name.of_string "ocamlfind", Atom (Filter (FIdent ([],OpamVariable.of_string "build",None)))))) in
  let depopts = nixdeps_of_depopts ~refnames opam.depopts in
  let deps = NixDeps.union deps depopts in
  let conflicts = nixdeps_of_depopts ~refnames opam.conflicts in
  (* TODO handle conflict_class *)
  (* TODO handle available *)
  (* TODO handle flags *)
  (* TODO handle env *)
  let build = nix_expr_of_commands opam.build in
  (* TODO handle run_test *)
  let install = nix_expr_of_commands opam.install in
  (* TODO handle remove *)
  (* TODO handle substs *)
  let extra_patches = OpamStd.Option.default [] @@
    OpamPackage.Name.Map.find_opt name patches
  in
  let patches = nix_expr_of_patches (opam.patches @ extra_patches) in
  (* TODO handle build_env *)
  (* TODO handle features *)
  (* TODO handle messages *)
  (* TODO handle post_messages *)
  (* TODO handle depexts *)
  let extra_depexts = OpamStd.Option.map_default (fun x -> [x,FBool true]) [] @@
    OpamPackage.Name.Map.find_opt name extra_depexts
  in
  let depexts = nixdeps_of_depexts (opam.depexts @ extra_depexts) in
  let deps = NixDeps.union deps depexts in
  (* TODO handle libraries *)
  (* TODO handle syntax *)
  (* TODO handle dev_repo *)
  (* TODO handle pin_depends *)
  (* TODO handle maintainer *)
  (* TODO handle author *)
  (* TODO handle tags *)
  (* TODO handle homepage *)
  (* TODO handle doc *)
  (* TODO handle bug_reports *)
  (* TODO handle extensions *)
  let (src, extra_src, uses_zip) = nix_src_of_opam name version opam in
  let extra_files = match opam.extra_files with | None -> [] | Some xs -> xs in
  let extra_src = extra_src @ List.map (fun (b,_) -> (b, nix_path @@ OpamFilename.Base.to_string b)) extra_files in
  (* TODO handle descr *)
  (* TODO handle metadata_dir *)
  { pname = name; version; deps; prop_deps = deps; conflicts; build; install; patches; src; extra_src; uses_zip }

(* NIXIFY *)
let nixify_doc = "Generates nix expressions from $(i,opam) files."
let nixify =
  let doc = nixify_doc in
  let man = [
    `S "DESCRIPTION";
    `P "Given an $(i,opam) file,  \
        outputs recommendations, warnings or errors on stderr."
  ] in
  let files =
    Arg.(value & pos_all (OpamArg.existing_filename_dirname_or_dash) [] &
         info ~docv:"FILES" []
           ~doc:"Name of the opam files to check, or directory containing \
                 them. Current directory if unspecified")
  in
  let warnings =
    OpamArg.mk_opt ["warnings";"W"] "WARNS"
      "Select the lint warnings to show or hide. $(i,WARNS) should be a \
       concatenation of $(b,+N), $(b,-N), $(b,+N..M), $(b,-N..M) to \
       respectively enable or disable warning or error number $(b,N) or \
       all warnings with numbers between $(b,N) and $(b,M) inclusive.\n\
       All warnings are enabled by default, unless $(i,WARNS) starts with \
       $(b,+), which disables all but the selected ones."
      OpamArg.warn_selector []
  in
  let package =
    OpamArg.mk_opt ["package"] "PKG"
      "Lint the current definition of the given package instead of specifying \
       an opam file directly."
      Arg.(some OpamArg.package) None
  in
  let refnames =
    OpamArg.mk_opt_all ["refname";"r"] "OLDNAME:NEWNAME"
      "Refer to the package $(i,OLDNAME) as $(i,NEWNAME).  This renaming will \
       occur in the argument lists of all packages that make use of $(i,OLDNAME). \
       It does not affect the name used as a variable when building $(i,OLDNAME) \
       or in the derivation metadata for $(i,OLDNAME)."
      Arg.(pair ~sep:':' OpamArg.package_name OpamArg.package_name)
  in
  let patches =
    OpamArg.mk_opt_all ["patch";"p"] "NAME:PATCH"
      "Apply an additional patch to $(i,NAME) after all the patches specified \
       in the repository's metadata.  Ignored when processing packages other \
       than $(i,NAME)."
      Arg.(pair ~sep:':' OpamArg.package_name string)
  in
  let depexts =
    OpamArg.mk_opt_all ["depext";"x"] "PACKAGE:NIXPACKAGE"
      "Create a dependency from the OPAM package $(i,PACKAGE) to the nix package \
       $(i,NIXPACKAGE), specified by attribute name.  Ignored when processing \
       packages other than $(i,PACKAGE)."
      Arg.(pair ~sep:':' OpamArg.package_name OpamArg.package_name)
  in
  let nixify global_options files package warnings_sel refnames patches extra_depexts =
    OpamArg.apply_global_options global_options;
    let refnames = OpamPackage.Name.Map.of_list refnames in
    let patches = OpamPackage.Name.Map.(List.fold_right (fun (pk,pt) -> update pk (List.cons (OpamFilename.Base.of_string pt, None)) []) patches empty) in
    let extra_depexts = OpamPackage.Name.Map.(List.fold_right (fun (pk,px) -> update pk (List.cons (OpamPackage.Name.to_string px)) []) extra_depexts empty) in
    let opam_files_in_dir d =
      match OpamPinned.files_in_source d with
      | [] ->
        OpamConsole.warning "No opam files found in %s"
          (OpamFilename.Dir.to_string d);
        []
      | l ->
        List.map (fun (_name,f) -> Some f) l
    in
    let files = match files, package with
      | [], None -> (* Lookup in cwd if nothing was specified *)
        opam_files_in_dir (OpamFilename.cwd ())
      | files, None ->
        List.map (function
            | None -> [None] (* this means '-' was specified for stdin *)
            | Some (OpamFilename.D d) ->
              opam_files_in_dir d
            | Some (OpamFilename.F f) ->
              [Some (OpamFile.make f)])
          files
        |> List.flatten
      | [], Some pkg ->
        (OpamGlobalState.with_ `Lock_none @@ fun gt ->
         OpamSwitchState.with_ `Lock_none gt @@ fun st ->
         try
           let nv = match pkg with
             | name, Some v -> OpamPackage.create name v
             | name, None -> OpamSwitchState.get_package st name
           in
           let opam = OpamSwitchState.opam st nv in
           match OpamPinned.orig_opam_file opam with
           | None -> raise Not_found
           | some -> [some]
         with Not_found ->
           OpamConsole.error_and_exit `Not_found "No opam file found for %s%s"
             (OpamPackage.Name.to_string (fst pkg))
             (match snd pkg with None -> ""
                               | Some v -> "."^OpamPackage.Version.to_string v))
      | _::_, Some _ ->
        OpamConsole.error_and_exit `Bad_arguments
          "--package and a file argument are incompatible"
    in
    let msg = OpamConsole.errmsg in
    let err =
      List.fold_left (fun err opam_f ->
          try
            let warnings,opam =
              match opam_f with
              | Some f -> OpamFileTools.lint_file f
              | None ->
                OpamFileTools.lint_channel
                  (OpamFile.make (OpamFilename.of_string "-")) stdin
            in
            let enabled =
              let default = match warnings_sel with
                | (_,true) :: _ -> false
                | _ -> true
              in
              let map =
                List.fold_left
                  (fun acc (wn, enable) -> OpamStd.IntMap.add wn enable acc)
                  OpamStd.IntMap.empty warnings_sel
              in
              fun w -> try OpamStd.IntMap.find w map with Not_found -> default
            in
            let warnings = List.filter (fun (n, _, _) -> enabled n) warnings in
            let failed =
              List.exists (function _,`Error,_ -> true | _ -> false) warnings
            in
            if warnings <> [] then
              msg "%s%s\n%s\n"
                (OpamStd.Option.to_string (fun f -> OpamFile.to_string f ^ ": ")
                   opam_f)
                (if failed then OpamConsole.colorise `red "Errors."
                 else OpamConsole.colorise `yellow "Warnings.")
                (OpamFileTools.warns_to_string warnings);
            opam
              |> OpamStd.Option.map OpamFormatUpgrade.opam_file_from_1_2_to_2_0
              |> OpamStd.Option.iter (fun upgraded ->
                   Format.printf "%a" (pp_nix_pkg ~opam:upgraded) @@ nix_of_opam ~refnames ~patches ~extra_depexts upgraded);
            err || failed
          with
          | Parsing.Parse_error
          | OpamLexer.Error _
          | OpamPp.Bad_format _ ->
            msg "File format error\n";
            true)
        false files
    in
    if err then OpamStd.Sys.exit_because `False
  in
  Term.(const nixify $OpamArg.global_options $files $package $warnings $refnames $patches $depexts),
  OpamArg.term_info "opam-nixify" ~doc ~man

let () =
  OpamStd.Sys.at_exit (fun () ->
      flush stderr;
      flush stdout;
    );
  run (nixify)
