type id = string

type exp =
    Var of id
  | FieldGet of exp * id
  | FieldSet of exp * id * exp
  | MethodCall of exp * id * exp list
  | New of id * exp list
  | Cast of id * exp

module Type = struct
  type t = id

  let make (n : id) : t = n
  let name (t : t) : id = t
end

module Field = struct
  type t = {
    name: id;
    ty: Type.t;
  }

  let name f = f.name
  let ty f = f.ty
end

module Constructor = struct
  type t = {
    name: id;
    parameters: (id * Type.t) list;
    body: exp list;
    super_arguments: exp list;
  }

  let name c = c.name
  let parameters c = c.parameters
  let body c = c.body
  let super_arguments c = c.super_arguments
end

module Method = struct
  type t = {
    name: id;
    parameters: (id * Type.t) list;
    body: exp;
    return_type: Type.t;
  }
end

module Class = struct
  type t = {
    name: id;
    super: Type.t;
    fields: Field.t list;
    constructor: Constructor.t;
    methods: Method.t list;
  }

  let ty c = Type.make c.name
  let name c = c.name
  let super c = c.super
  let fields c = c.fields
  let constructor c = c.constructor
  let methods c = c.methods
end

type program = Class.t list
