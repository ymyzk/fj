type id = string

type exp =
    Var of id
  | FieldGet of exp * id
  | FieldSet of exp * id * exp
  | MethodCall of exp * id * exp list
  | New of id * exp list
  | Cast of id * exp

module Field = struct
  type t = {
    name: id;
    klass: id;
  }

  let name c = c.name
  let klass c = c.klass
end

module Constructor = struct
  type t = {
    name: id;
    parameters: (id * id) list;
    body: exp list;
    super_arguments: exp list;
  }
end

module Method = struct
  type t = {
    name: id;
    parameters: (id * id) list;
    body: exp;
    return_type: id;
  }
end

module Class = struct
  type t = {
    name: id;
    super: id;
    fields: Field.t list;
    constructor: Constructor.t;
    methods: Method.t list;
  }

  let name c = c.name
  let super c = c.super
  let fields c = c.fields
  let constructor c = c.constructor
  let methods c = c.methods
end

type program = Class.t list
