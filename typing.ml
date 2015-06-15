open Syntax

module ClassTable = Map.Make (
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

(* get_class : Class.t ClassTable.t -> id -> Class.t *)
let get_class table name =
  try
    ClassTable.find name table
  with Not_found ->
    raise (Type_error ("the class is not found in table: " ^ name))

(* create_classtable : Class.t list -> Class.t ClassTable.t *)
let create_classtable classes =
  let table = ClassTable.singleton (Class.name base_class) base_class in
  List.fold_left begin fun tb klass ->
    let name = Class.name klass in
    if ClassTable.mem name tb then
      raise (Type_error ("duplicate class: " ^ name))
    else
      ClassTable.add name klass tb
  end table classes

(* check_class_super : Class.t ClassTable.t -> id -> unit *)
let check_class_super table name =
  let rec check_class_super table name1 name2 =
    if name1 = name2 then
      raise (Type_error ("cyclic inheritance involving " ^ name1))
    else if name1 = base_class_name || name2 = base_class_name then
      ()
    else
      check_class_super table name1 (Class.super (get_class table name2))
  in
    check_class_super table name (Class.super (get_class table name))

(* check_class : Class.t ClassTable.t -> Class.t -> unit *)
let check_class table klass =
  check_class_super table (Class.name klass)

(* check_classtable : Class.t ClassTable.t -> unit *)
let check_classtable table =
  ClassTable.iter (fun _ k -> check_class table k) table

(* check : Class.t list -> unit *)
let check classes =
  let table = create_classtable classes in
  (* For debugging *)
  ClassTable.iter (fun name klass -> print_endline name) table;
  check_classtable table
