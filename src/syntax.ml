module Id = struct
  type t = {
    name: string;
    position: Lexing.position
  }

  let make n = { name = n; position = Lexing.dummy_pos; }

  let name i = i.name
  let position i = i.position
end

type exp =
    Var of Id.t
  | FieldGet of exp * Id.t
  | FieldSet of exp * Id.t * exp
  | MethodCall of exp * Id.t * exp list
  | New of Id.t * exp list
  | Cast of Id.t * exp

module Type = struct
  type t = string

  let make (n : t) : t = n

  let name (t : t) : t = t
end

module Field = struct
  type t = {
    name: Id.t;
    ty: Type.t;
  }

  let name f = Id.name f.name
  let position f = Id.position f.name
  let ty f = f.ty
end

module Constructor = struct
  type t = {
    name: Id.t;
    parameters: (Id.t * Type.t) list;
    body: exp list;
    super_arguments: exp list;
  }

  let name c = Id.name c.name
  let position c = Id.position c.name
  let parameters c = c.parameters
  let body c = c.body
  let super_arguments c = c.super_arguments

  let parameter_names c = List.map (fun p -> Id.name (fst p)) c.parameters
  let parameter_types c = List.map snd c.parameters
end

module Method = struct
  type t = {
    name: Id.t;
    parameters: (Id.t * Type.t) list;
    body: exp;
    return_type: Type.t;
  }

  let name m = Id.name m.name
  let parameters m = m.parameters
  let body m = m.body
  let return_type m = m.return_type

  let parameter_types m = List.map snd m.parameters
end

module Class = struct
  type t = {
    name: Id.t;
    super: Type.t;
    fields: Field.t list;
    constructor: Constructor.t;
    methods: Method.t list;
  }

  let ty c = Type.make (Id.name c.name)
  let position c = Id.position c.name
  let name c = Id.name c.name
  let super c = c.super
  let fields c = c.fields
  let constructor c = c.constructor
  let methods c = c.methods
end

type program = Class.t list
