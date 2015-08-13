open Syntax

module Environment = Map.Make (
  struct
    type t = id
    let compare = compare
  end
)

exception Type_error of string

let base_class_name = "Object"
let base_class = {
  Class.name = base_class_name;
  super = "???";
  fields = [];
  constructor = {
    Constructor.name = base_class_name;
    parameters = [];
    body = [];
    super_arguments = [];
  };
  methods = [];
}

(* get_class : Class.t Environment.t -> id -> Class.t *)
let get_class table name =
  try
    Environment.find name table
  with Not_found ->
    raise (Type_error ("the class is not found in table: " ^ name))

let super_of table klass =
  get_class table (Type.name (Class.super klass))

(* k1 は k0 のサブクラスかどうか *)
let rec is_subclass table k0 k1 =
  if k0 = k1 then
    true
  else if k1 = base_class_name then
    false
  else
    is_subclass table k0 (Class.super (get_class table k1))

(* l1 と l0 のそれぞれ n 番目の要素 k1, k0 について, k1 は k0 のサブクラスかどうか *)
(* is_subclasses : Class.t Environment.t -> Type.t list -> Type.t list -? bool *)
let rec is_subclasses table l0 l1 =
  match l0, l1 with
  | [], [] -> true
  | [], _ -> false
  | _, [] -> false
  | (k0 :: ks0), (k1 :: ks1) ->
      begin
        print_endline ((Type.name k1) ^ " <? " ^ (Type.name k0));
      (is_subclass table k0 k1) && (is_subclasses table ks0 ks1)
      end

(* get_field : Class.t -> id -> Class.t *)
let rec get_field table klass name =
  try
    List.find (fun f -> Field.name f = name) (Class.fields klass)
  with Not_found ->
    if Class.ty klass = Class.ty base_class then
      raise (Type_error ("the field '" ^ name ^ "' is not found in class: " ^ (Class.name klass)));
    let super_klass = super_of table klass in
    get_field table super_klass name

(* create_classtable : Class.t list -> Class.t Environment.t *)
let create_classtable classes =
  let table = Environment.singleton (Class.name base_class) base_class in
  List.fold_left begin fun tb klass ->
    let name = Class.name klass in
    if Environment.mem name tb then
      raise (Type_error ("duplicate class: " ^ name))
    else
      Environment.add name klass tb
  end table classes

let rec check_exp table env = function
  | Var(n0) when Environment.mem n0 env ->
      Environment.find n0 env
  | Var(n0) ->
      raise (Type_error ("'" ^ n0 ^ "' is not found in the environment"))
  | FieldSet(e0, n0, e1) ->
      let k0 = get_class table (check_exp table env e0) in
      let f0 = get_field table k0 n0 in
      let t0 = Field.ty f0 in
      let t1 = check_exp table env e1 in
      if not (is_subclass table t0 t1) then
        raise (Type_error ("'" ^ t0 ^ "' is not a subclass of '" ^ t1 ^ "'"));
      t0
  | _ -> raise (Type_error "not implemented")

(* check_class_super : Class.t Environment.t -> id -> unit *)
let check_class_super table name =
  let rec check_class_super table name1 name2 =
    let super_name = Class.super (get_class table name2) in
    if name1 = name2 || name2 = super_name then
      raise (Type_error ("cyclic inheritance involving " ^ name1))
    else if name2 = base_class_name then
      ()
    else
      check_class_super table name1 super_name
  in
    check_class_super table name (Class.super (get_class table name))

(* check_field : Type.t Environment.t -> Class.t -> Field.t -> unit *)
(* フィールド名の重複チェック *)
let check_field env klass field =
  let name = Field.name field in
  if Environment.mem name env then
    raise (Type_error ("variable " ^ name ^ " is already defined in class " ^ (Class.name klass)));
  ()

(* check_fields : Type.t Environment.t -> Class.t -> Type.t Environment.t *)
(* クラスのフィールドのチェックと env へのフィールドの追加 *)
let check_fields env klass =
  List.fold_left begin
    fun env' field ->
      let name = Field.name field in
      let ty = Field.ty field in
      check_field env' klass field;
      Environment.add name ty env'
  end env (Class.fields klass)

(* check_constructor *)
let rec check_constructor table env klass =
  let constructor = Class.constructor klass in
  (* コンストラクタの名前はクラスの名前と同じ *)
  if Class.name klass <> Constructor.name constructor then
    raise (Type_error ("invalid constructor name"));
  let parameters = Constructor.parameters constructor in
(*   List.iter (fun (p, ty) -> print_endline p) parameters; *)
  (* コンストラクタのパラメーターを左から順に環境に追加 *)
  let env' = List.fold_left (fun e (k, n) -> Environment.add k n e) env parameters in
(*
  print_endline "ENV";
  Environment.iter (fun name klass -> print_endline (name ^ " " ^ klass)) env';
  print_endline "/ENV";
*)
  (* コンストラクタ本体の型チェック *)
  List.iter (fun e -> ignore (check_exp table env' e)) (Constructor.body constructor);
  if not (Class.ty klass = Class.ty base_class) then
    begin
      (* スーパークラスのコンストラクタ呼び出しの型チェック *)
      let super_klass = super_of table klass in
(*
      print_endline ("CLASS: " ^ (Class.name klass));
      print_endline ("SUPER: " ^ (Class.name super_klass));
*)
      let super_arguments = Constructor.super_arguments constructor in
      let super_arguments_types = List.map (check_exp table env') super_arguments in
      let super_parameters_types = List.map (fun (_, ty) -> ty) (Constructor.parameters (Class.constructor super_klass)) in
(*
      if List.length super_arguments_types <> List.length super_parameters_types then
        raise (Type_error "super error")
*)
      List.iter (fun tp -> print_endline tp) super_arguments_types;
      List.iter (fun tp -> print_endline tp) super_parameters_types;
      check_constructor table env super_klass
    end;
  ()

(* check_class : Class.t Environment.t -> Class.t Environment.t -> Class.t -> unit *)
let check_class table env klass =
  check_class_super table (Class.name klass);
  let env' = Environment.add "this" (Class.ty klass) env in
  let env' = check_fields env' klass in
  check_constructor table env' klass

(* check_classtable : Class.t Environment.t -> unit *)
let check_classtable table =
  let env = Environment.empty in
  Environment.iter (fun _ k -> check_class table env k) table

(* check : Class.t list -> unit *)
let check classes =
  let table = create_classtable classes in
  (* For debugging *)
(*   Environment.iter (fun name klass -> print_endline name) table; *)
  check_classtable table
