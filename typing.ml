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

(* check : klass list -> ????? *)
let check classes =
  let table = create_classtable classes in
  (* For debugging *)
  ClassTable.iter (fun name klass -> print_endline name) table;
  classes
