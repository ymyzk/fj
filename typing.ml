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
  name = base_class_name;
  super = "???";
  fields = [];
  constructor = {
    name = base_class_name;
    parameters = [];
    body = [];
    super_arguments = [];
  };
  methods = [];
}

(* get_class : ClassTable -> id -> klass *)
let get_class table name =
  try
    ClassTable.find name table
  with Not_found ->
    raise (Type_error ("the class is not found in table: " ^ name))

(* create_classtable : klass list -> ClassTable *)
let create_classtable classes =
  let table = ClassTable.singleton base_class.name base_class in
  List.fold_left begin fun tb klass ->
    let name = klass.name in
    if ClassTable.mem name tb then
      raise (Type_error ("duplicate class: " ^ name))
    else
      ClassTable.add name klass tb
  end table classes

(* check_class_super : ClassTable -> id -> unit *)
let check_class_super table name =
  let rec check_class_super table name1 name2 =
    if name1 = name2 then
      raise (Type_error ("cyclic inheritance involving " ^ name1))
    else if name1 = base_class_name || name2 = base_class_name then
      ()
    else
      check_class_super table name1 ((get_class table name2).super)
  in
    check_class_super table name ((get_class table name).super)

(* check_class : ClassTable -> klass -> unit *)
let check_class table klass =
  check_class_super table klass.name

(* check_classtable : ClassTable -> unit *)
let check_classtable table =
  ClassTable.iter (fun _ k -> check_class table k) table

(* check : klass list -> unit *)
let check classes =
  let table = create_classtable classes in
  (* For debugging *)
  ClassTable.iter (fun name klass -> print_endline name) table;
  check_classtable table
