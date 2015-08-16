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

(* クラスのリストからクラステーブルを作成する *)
(* Class.t list -> Class.t Environment.t *)
let create_classtable classes =
  let table = Environment.singleton (Class.name base_class) base_class in
  List.fold_left begin fun tb klass ->
    let name = Class.name klass in
    if Environment.mem name tb then
      (* 同じ名前のクラス名が複数あればエラー *)
      raise (Type_error ("duplicate class: " ^ name))
    else
      Environment.add name klass tb
  end table classes

(* クラステーブルからクラスを取得する *)
(* Class.t Environment.t -> Type.t -> Class.t *)
let get_class table name =
  try
    Environment.find name table
  with Not_found ->
    raise (Type_error ("the class is not found in table: " ^ name))

(* クラステーブルからスーパークラスを取得する *)
(* Class.t Environment.t -> Class.t -> Class.t *)
let super_of table klass =
  get_class table (Type.name (Class.super klass))

(* k1 は k0 のサブクラスかどうか *)
(* Class.t Environment.t -> Type.t -> Type.t -> bool *)
let rec is_subclass table k0 k1 =
  if k0 = k1 then
    (* C <: C *)
    true
  else if k1 = base_class_name then
    (* Object は Object 以外のサブクラスではない *)
    false
  else
    (* (class C extends D -> C <: D) & D <: E -> C <: E より,
     * D <: E をチェックする *)
    is_subclass table k0 (Class.super (get_class table k1))

(* l1 と l0 の n 番目の要素 k1, k0 について, k1 は k0 のサブクラスかどうか *)
(* Class.t Environment.t -> Type.t list -> Type.t list -> bool *)
let rec is_subclasses table l0 l1 =
  match l0, l1 with
  | [], [] -> true
  | [], _  -> false
  | _ , [] -> false
  | (k0 :: ks0), (k1 :: ks1) ->
      (is_subclass table k0 k1) && (is_subclasses table ks0 ks1)

(* クラスからフィールドを取得する処理 *)
(* Class.t Environment.t -> Class.t -> id -> Field.t *)
let get_field table klass name =
  let rec get_field table klass' name =
    try
      List.find (fun f -> Field.name f = name) (Class.fields klass')
    with Not_found ->
      if Class.ty klass' = Class.ty base_class then
        raise (Type_error ("the field '" ^ name ^ "' is not found in class: " ^ (Class.name klass)));
      let super_klass = super_of table klass' in
      get_field table super_klass name in
  get_field table klass name

(* クラスからメソッドを取得する処理 *)
(* Class.t Environment.t -> Class.t -> id -> Method.t *)
let get_method table klass name =
  let rec get_method table klass' name =
    try
      List.find (fun m -> Method.name m = name) (Class.methods klass')
    with Not_found ->
      if Class.ty klass' = Class.ty base_class then
        raise (Type_error ("the method '" ^ name ^ "' is not found in class: " ^ (Class.name klass)));
      let super_klass = super_of table klass' in
      get_method table super_klass name in
  get_method table klass name

(* 式の型を求める処理 *)
(* Class.t Environment.t -> Type.t Environment.t -> exp -> Type.t *)
let rec check_exp table env = function
  | Var(n0) when Environment.mem n0 env ->
      Environment.find n0 env
  | Var(n0) ->
      raise (Type_error ("'" ^ n0 ^ "' is not found in the environment"))
  | FieldGet(e0, n0) -> (* e0.n0 *)
      let k0 = get_class table (check_exp table env e0) in
      let f0 = get_field table k0 n0 in
      Field.ty f0
  | FieldSet(e0, n0, e1) -> (* e0.n0 = e1 *)
      begin match e0, n0, e1 with
      | (Var "this"), _, (Var _) ->
        let k0 = get_class table (check_exp table env e0) in (* e0 (this) : k0 *)
        let f0 = get_field table k0 n0 in
        let t0 = Field.ty f0 in (* e0.n0 : t0 *)
        let t1 = check_exp table env e1 in (* e1 : t1 *)
        if not (is_subclass table t0 t1) then
          raise (Type_error ("'" ^ t1 ^ "' is not a subclass of '" ^ t0 ^ "'"));
        t0
      | _, _, _ -> raise (Type_error "not supported")
      end
  | MethodCall(e0, n0, ps0) -> (* e0.m0(ps0) *)
      let ts0 = List.map (check_exp table env) ps0 in
      let k0 = get_class table (Type.name (check_exp table env e0)) in
      let m0 = get_method table k0 n0 in
      let ts1 = List.map snd (Method.parameters m0) in
      if is_subclasses table ts1 ts0 then
        Method.return_type m0
      else
        raise (Type_error (
          "cannot invoke a method " ^ n0))
  | New(t0, ps0) -> (* new t0(ps0) *)
      let pt0 = List.map (check_exp table env) ps0 in
      let k0 = get_class table t0 in
      let pt1 = List.map snd (Constructor.parameters (Class.constructor k0)) in
      if is_subclasses table pt1 pt0 then
        Class.ty k0
      else
        raise (Type_error (
          "cannot invoke a class constructor " ^ (Class.name k0)))
  | Cast(t0, e0) -> (* (t0)e0 *)
      let t1 = check_exp table env e0 in
      if is_subclass table t0 t1 then
        (* up cast *)
        t0
      else if is_subclass table t1 t0 then
        (* down cast *)
        t0
      else
        (* stupid cast *)
        begin
          (* エラーメッセージを出力するが処理は続ける *)
          Printf.eprintf
            "stupid cast from class '%s' to class '%s'\n"
            (Type.name t1) (Type.name t0);
          t0
        end

(* クラスの部分型関係のチェック, 不完全? 不要? *)
(* Class.t Environment.t -> id -> unit *)
let check_class_super table name =
  let rec check_class_super table name1 name2 =
    if name1 = base_class_name || name2 = base_class_name then
      (* 前者は特別に Class table に入れている Object のため *)
      ()
    else
      let super_name = Class.super (get_class table name2) in
      if name1 = name2 || name2 = super_name then
        raise (Type_error ("cyclic inheritance involving " ^ name1))
      else
        check_class_super table name1 super_name
  in
    check_class_super table name (Class.super (get_class table name))

(* フィールド名の重複チェック *)
(* Type.t Environment.t -> Class.t -> Field.t -> unit *)
let check_field env klass field =
  let name = Field.name field in
  if Environment.mem name env then
    raise (Type_error ("variable " ^ name ^ " is already defined in class " ^ (Class.name klass)));
  ()

(* クラスのフィールドのチェックと env へのフィールドの追加 *)
(* Type.t Environment.t -> Class.t -> Type.t Environment.t *)
let check_fields env klass =
  List.fold_left begin
    fun env' field ->
      let name = Field.name field in
      let ty = Field.ty field in
      check_field env' klass field;
      Environment.add name ty env'
  end env (Class.fields klass)

(* 初期化されていないフィールドがないかチェック *)
(* Class.t -> unit *)
let check_uninitialized_fields klass =
  let constructor_fields =
    List.map
      (function FieldSet(_, n, _) -> n | _ -> raise (Type_error "unknown"))
      (Constructor.body (Class.constructor klass)) in
  let constructor_fields = List.sort compare constructor_fields in
  let class_fields = List.map Field.name (Class.fields klass) in
  let class_fields = List.sort compare class_fields in
  (* コンストラクタ内で初期化されているフィールドの名前のリストと
   * クラスで定義されているフィールドの名前のリストを比較 *)
  if constructor_fields <> class_fields then
    raise (Type_error ("uninialized fields in class " ^ (Class.name klass)))
  else
    ()

(* コンストラクタのパラメーターを左から順に環境に追加 *)
(* Type.t Environment.t -> Constructor.t -> Type.t Environment.t *)
let check_constructor_parameters env constructor =
  let parameters = Constructor.parameters constructor in
  List.fold_left
    (fun e (k, n) -> Environment.add k n e) env parameters

(* すべてのパラメータがフィールドの初期化かスーパークラスのコンストラクタ呼び出しに利用されているかをチェック *)
(* Constructor.t -> unit *)
let check_constructor_parameters_used constructor =
  let parameters =
      List.map fst (Constructor.parameters constructor) in
  let parameters = List.sort compare parameters in
  let fields =
    List.map
      (function FieldSet(_, _, Var(n)) -> n | _ -> raise (Type_error "unknown"))
      (Constructor.body constructor) in
  let fields = List.sort compare fields in
  let arguments =
    List.map
      (function Var(n) -> n | _ -> raise (Type_error "unknown"))
      (Constructor.super_arguments constructor) in
  let fields_and_arguments = List.sort compare (fields @ arguments) in
  if parameters <> fields_and_arguments then
    (* コンストラクタの引数で利用されていないものがある *)
    raise (Type_error (
      "incorrect parameter use in the constructor of class " ^ (Constructor.name constructor)))
  else
    ()

(* コンストラクタ本体 (フィールドの初期化) の型チェック *)
(* Class.t Environment.t Type.t Environment.t Constructor.t -> unit *)
let check_constructor_fields table env constructor =
  List.iter
    (fun e -> ignore (check_exp table env e))
    (Constructor.body constructor)

(* スーパークラスのコンストラクタ呼び出しの型チェック *)
(* Class.t Environment.t -> Type.t Environment.t -> Class.t -> unit *)
let check_constructor_super table env klass =
  let constructor = Class.constructor klass in
  if Class.ty klass = Class.ty base_class then
    (* Object クラスはスーパークラスのコンストラクタを呼び出さない *)
    ()
  else
    (* クラスの型が Object でなければ *)
    (* スーパークラスを取得 *)
    let super_klass = super_of table klass in
    (* スーパークラスのコンストラクタに渡される引数を取得 *)
    let arguments = Constructor.super_arguments constructor in
    (* スーパークラスのコンストラクタに渡される引数の型を取得 *)
    let argument_types = List.map (check_exp table env) arguments in
    (* スーパークラスのコンストラクタの引数の型を取得 *)
    let parameter_types = List.map snd (Constructor.parameters (Class.constructor super_klass)) in
    if List.length argument_types <> List.length parameter_types then
      (* 引数の数が正しくない *)
      raise (Type_error (
        "super: argument lists differ in length for class "
        ^ (Class.name super_klass)))
    else if is_subclasses table parameter_types argument_types then
      (* OK *)
      ()
    else
      (* スーパークラスのコンストラクタと型が合わない *)
      raise (Type_error (
        "cannot invoke a super class constructor in the constructor of class "
        ^ (Class.name super_klass)))

(* コンストラクタのチェック *)
(* Class.t Environment.t -> Type.t Environment.t -> Class.t -> unit *)
let rec check_constructor table env klass =
  let constructor = Class.constructor klass in
  (* コンストラクタの名前はクラスの名前と同じ *)
  if Class.name klass <> Constructor.name constructor then
    raise (Type_error ("invalid constructor name"));
  (* パラメータのチェックと環境への追加 *)
  let env' = check_constructor_parameters env constructor in
  (* フィールドの初期化のチェック *)
  check_constructor_fields table env' constructor;
  (* スーパークラスコンストラクタ呼び出しのチェック *)
  check_constructor_super table env' klass;
  (* 全てのパラメータが重複なく利用されているかチェック *)
  check_constructor_parameters_used constructor;
  ()

(* メソッドのチェック *)
(* Class.t Environment.t Type.t Environment.t -> Method.t -> unit *)
let check_method table env meth =
  (* パラメータを環境に追加 *)
  let env' =
    List.fold_left
      (fun e (n, t) -> Environment.add n t e)
      env (Method.parameters meth) in
  (* メソッドの内部の型をチェック *)
  let ty = check_exp table env' (Method.body meth) in
  (* 戻り値と型が合うかどうか *)
  if is_subclass table (Method.return_type meth) ty then
    ()
  else
    raise (Type_error ("return type error"))

(* クラスのすべてのメソッドのチェック *)
(* Class.t Environment.t -> Type.t Environment.t -> Class.t -> unit *)
let check_methods table env klass =
  List.iter (check_method table env) (Class.methods klass)

(* クラスのチェック *)
(* Class.t Environment.t -> Type.t Environment.t -> Class.t -> unit *)
let check_class table env klass =
  check_class_super table (Class.name klass);
  let env' = Environment.add "this" (Class.ty klass) env in
  (* フィールドのチェック *)
  let env' = check_fields env' klass in
  (* 初期化されていないフィールドが存在しないかのチェック *)
  check_uninitialized_fields klass;
  (* コンストラクタのチェック *)
  check_constructor table env' klass;
  (* メソッドのチェック *)
  check_methods table env' klass

(* クラステーブルのクラスを順にチェック *)
(* Class.t Environment.t -> unit *)
let check_classtable table =
  let env = Environment.empty in
  Environment.iter (fun _ k -> check_class table env k) table

(* クラスのリストを順にチェック *)
(* check : Class.t list -> unit *)
let check classes =
  let table = create_classtable classes in
  check_classtable table
