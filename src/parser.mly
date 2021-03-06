%{
open Syntax
%}

%token <Syntax.Id.t> ID

%token EOF
%token COMMA PERIOD LBRACE LPAREN RBRACE RPAREN EQ SEMICOLON
%token CLASS EXTENDS NEW RETURN SUPER THIS

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    class_definition_list EOF { $1 }

class_definition_list :
    class_definition { [$1] }
  | class_definition class_definition_list { $1 :: $2 }

class_definition :
    CLASS ID EXTENDS ID LBRACE
        field_list
        constructor
        method_definition_list
    RBRACE {
      {
        Class.name = $2;
        super = Type.make (Id.name $4);
        fields = $6;
        constructor = $7;
        methods = $8;
      }
    }

field_list :
    { [] }
  | field_list field { $1 @ [$2] }

field :
    ID ID SEMICOLON { { Field.name = $2; ty = Type.make (Id.name $1); } }

constructor :
    ID LPAREN parameter_list_opt RPAREN LBRACE
        SUPER LPAREN argument_list_opt RPAREN SEMICOLON
        field_initializer_list
    RBRACE { { Constructor.name = $1; parameters = $3; body = $11; super_arguments = $8; } }

parameter_list_opt :
    { [] }
  | parameter_list { $1 }

parameter_list :
    parameter { [$1] }
  | parameter COMMA parameter_list { $1 :: $3 }

parameter :
    ID ID { ($2, Type.make (Id.name $1)) }

argument_list_opt :
    { [] }
  | argument_list { $1 }

argument_list :
    argument { [$1] }
  | argument COMMA argument_list { $1 :: $3 }

argument :
    expression { $1 }

field_initializer_list :
    { [] }
  | field_initializer SEMICOLON field_initializer_list { $1 :: $3 }

field_initializer :
    THIS PERIOD ID EQ ID { ($3, Var $5) }

method_definition_list :
    { [] }
  | method_definition method_definition_list { $1 :: $2 }

method_definition :
    ID ID LPAREN parameter_list_opt RPAREN LBRACE
        RETURN expression SEMICOLON
    RBRACE {
      {
        Method.name = $2;
        parameters = $4;
        body = $8;
        return_type = Type.make (Id.name $1);
      }
    }

expression :
    expression_1 { $1 }
  | NEW ID LPAREN argument_list_opt RPAREN { New ($2, $4) }
  | LPAREN ID RPAREN expression { Cast ($2, $4) }

expression_1 :
    ID { Var $1 }
  | THIS { Var (Id.make "this") }
  | expression_1 PERIOD ID { FieldGet ($1, $3) }
  | expression_1 PERIOD ID LPAREN argument_list_opt RPAREN { MethodCall ($1, $3, $5) }
