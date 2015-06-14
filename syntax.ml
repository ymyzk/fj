type id = string

type exp =
    Var of id
  | FieldGet of exp * id
  | FieldSet of exp * id * exp
  | MethodCall of exp * id * exp list
  | New of id * exp list
  | Cast of id * exp

type field = {
    name: id;
    klass: id;
}

type constructor = {
    name: id;
    parameters: (id * id) list;
    body: exp list;
    super_arguments: exp list;
}

type method_type = {
    name: id;
    parameters: (id * id) list;
    body: exp;
    return_type: id;
}

type klass = {
    name: id;
    super: id;
    fields: field list;
    constructor: constructor;
    methods: method_type list;
}

type program = klass list
