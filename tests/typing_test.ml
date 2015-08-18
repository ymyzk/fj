open OUnit2

open Syntax
open Typing

let test_create_classtable test_cxt =
  let classes = [
    {
      Class.name = Id.make "A";
      super = "Object";
      fields = [];
      constructor = {
        Constructor.name = Id.make "A";
        parameters = [];
        body = [];
        super_arguments = [];
      };
      methods = [];
    };
    {
      Class.name = Id.make "B";
      super = "A";
      fields = [];
      constructor = {
        Constructor.name = Id.make "B";
        parameters = [];
        body = [];
        super_arguments = [];
      };
      methods = [];
    }
  ] in
  let table = create_classtable classes in
  begin
    assert_equal (Environment.find "A" table) (List.hd classes);
    assert_equal (Environment.find "B" table) (List.nth classes 1)
  end

let test_is_subclass test_cxt =
  let classes = [
    {
      Class.name = Id.make "A";
      super = "Object";
      fields = [];
      constructor = {
        Constructor.name = Id.make "A";
        parameters = [];
        body = [];
        super_arguments = [];
      };
      methods = [];
    };
    {
      Class.name = Id.make "B";
      super = "A";
      fields = [];
      constructor = {
        Constructor.name = Id.make "B";
        parameters = [];
        body = [];
        super_arguments = [];
      };
      methods = [];
    };
    {
      Class.name = Id.make "C";
      super = "B";
      fields = [];
      constructor = {
        Constructor.name = Id.make "C";
        parameters = [];
        body = [];
        super_arguments = [];
      };
      methods = [];
    };
    {
      Class.name = Id.make "D";
      super = "A";
      fields = [];
      constructor = {
        Constructor.name = Id.make "D";
        parameters = [];
        body = [];
        super_arguments = [];
      };
      methods = [];
    }
  ] in
  let table = create_classtable classes in
  let type_o = Type.make "Object" in
  let type_a = Class.ty (get_class table "A") in
  let type_b = Class.ty (get_class table "B") in
  let type_c = Class.ty (get_class table "C") in
  let type_d = Class.ty (get_class table "D") in
  assert_equal (is_subclass table type_o type_o) true;
  assert_equal (is_subclass table type_o type_a) true;
  assert_equal (is_subclass table type_o type_b) true;
  assert_equal (is_subclass table type_o type_c) true;
  assert_equal (is_subclass table type_o type_d) true;
  assert_equal (is_subclass table type_a type_o) false;
  assert_equal (is_subclass table type_a type_a) true;
  assert_equal (is_subclass table type_a type_b) true;
  assert_equal (is_subclass table type_a type_c) true;
  assert_equal (is_subclass table type_a type_d) true;
  assert_equal (is_subclass table type_b type_o) false;
  assert_equal (is_subclass table type_b type_a) false;
  assert_equal (is_subclass table type_b type_b) true;
  assert_equal (is_subclass table type_b type_c) true;
  assert_equal (is_subclass table type_b type_d) false;
  assert_equal (is_subclass table type_c type_o) false;
  assert_equal (is_subclass table type_c type_a) false;
  assert_equal (is_subclass table type_c type_b) false;
  assert_equal (is_subclass table type_c type_c) true;
  assert_equal (is_subclass table type_c type_d) false;
  assert_equal (is_subclass table type_d type_o) false;
  assert_equal (is_subclass table type_d type_a) false;
  assert_equal (is_subclass table type_d type_b) false;
  assert_equal (is_subclass table type_d type_c) false;
  assert_equal (is_subclass table type_d type_d) true

let test_is_subclasses test_cxt =
  let classes = [
    {
      Class.name = Id.make "A";
      super = "Object";
      fields = [];
      constructor = {
        Constructor.name = Id.make "A";
        parameters = [];
        body = [];
        super_arguments = [];
      };
      methods = [];
    };
    {
      Class.name = Id.make "B";
      super = "A";
      fields = [];
      constructor = {
        Constructor.name = Id.make "B";
        parameters = [];
        body = [];
        super_arguments = [];
      };
      methods = [];
    }
  ] in
  let table = create_classtable classes in
  let type_o = Type.make "Object" in
  let type_a = Class.ty (get_class table "A") in
  let type_b = Class.ty (get_class table "B") in
  assert_equal (is_subclasses table [] []) true;
  assert_equal (is_subclasses table [] [type_a]) false;
  assert_equal (is_subclasses table [type_a] []) false;
  assert_equal (is_subclasses table [type_o] [type_a]) true;
  assert_equal (is_subclasses table [type_o; type_b] [type_a; type_a]) false;
  assert_equal (is_subclasses table [type_o; type_b] [type_a; type_b]) true

let test_get_field test_cxt =
  let field_o1 = {
    Field.name = Id.make "o1";
    ty = Type.make "Object";
  } in
  let field_o2 = {
    Field.name = Id.make "o2";
    ty = Type.make "Object";
  } in
  let field_a1 = {
    Field.name = Id.make "a1";
    ty = Type.make "A";
  } in
  let classes = [
    {
      Class.name = Id.make "A";
      super = "Object";
      fields = [field_o1];
      constructor = {
        Constructor.name = Id.make "A";
        parameters = [];
        body = [];
        super_arguments = [];
      };
      methods = [];
    };
    {
      Class.name = Id.make "B";
      super = "A";
      fields = [field_o2; field_a1];
      constructor = {
        Constructor.name = Id.make "B";
        parameters = [];
        body = [];
        super_arguments = [];
      };
      methods = [];
    }
  ] in
  let table = create_classtable classes in
  let class_a = get_class table "A" in
  let class_b = get_class table "B" in
  (* 同じクラス内のフィールドの取得 *)
  assert_equal (get_field table class_a "o1") field_o1;
  assert_equal (get_field table class_b "o2") field_o2;
  assert_equal (get_field table class_b "a1") field_a1;
  (* super まで探しに行く *)
  assert_equal (get_field table class_b "o1") field_o1;
  (* 存在しないフィールドの取得 *)
  assert_raises
    (Type_error (Lexing.dummy_pos, "the field 'o3' is not found in class: B"))
    (fun _ -> get_field table class_b "o3")

let test_get_method test_cxt =
  let type_o = Type.make "Object" in
  let type_a = Type.make "A" in
  let method_a1 = {
    Method.name = Id.make "a1";
    parameters = [];
    body = New(Id.make "Object", []);
    return_type = type_o;
  } in
  let method_b1 = {
    Method.name = Id.make "b1";
    parameters = [];
    body = New(Id.make "Object", []);
    return_type = type_o;
  } in
  let method_b2 = {
    Method.name = Id.make "b2";
    parameters = [];
    body = New(Id.make "A", []);
    return_type = type_a;
  } in
  let classes = [
    {
      Class.name = Id.make "A";
      super = "Object";
      fields = [];
      constructor = {
        Constructor.name = Id.make "A";
        parameters = [];
        body = [];
        super_arguments = [];
      };
      methods = [method_a1];
    };
    {
      Class.name = Id.make "B";
      super = "A";
      fields = [];
      constructor = {
        Constructor.name = Id.make "B";
        parameters = [];
        body = [];
        super_arguments = [];
      };
      methods = [method_b1; method_b2];
    }
  ] in
  let table = create_classtable classes in
  let class_a = get_class table "A" in
  let class_b = get_class table "B" in
  (* 同じクラス内のメソッドの取得 *)
  assert_equal (get_method table class_a "a1") method_a1;
  assert_equal (get_method table class_b "b1") method_b1;
  assert_equal (get_method table class_b "b2") method_b2;
  (* super まで探しに行く *)
  assert_equal (get_method table class_b "a1") method_a1;
  (* 存在しないメソッドの取得 *)
  assert_raises Not_found (fun _ -> get_method table class_b "c1")

let test_check_class_super test_cxt =
  let classes = [
    {
      Class.name = Id.make "A";
      super = "A";
      fields = [];
      constructor = {
        Constructor.name = Id.make "A";
        parameters = [];
        body = [];
        super_arguments = [];
      };
      methods = [];
    };
    {
      Class.name = Id.make "B";
      super = "A";
      fields = [];
      constructor = {
        Constructor.name = Id.make "B";
        parameters = [];
        body = [];
        super_arguments = [];
      };
      methods = [];
    };
    {
      Class.name = Id.make "C";
      super = "Object";
      fields = [];
      constructor = {
        Constructor.name = Id.make "C";
        parameters = [];
        body = [];
        super_arguments = [];
      };
      methods = [];
    };
    {
      Class.name = Id.make "D";
      super = "C";
      fields = [];
      constructor = {
        Constructor.name = Id.make "D";
        parameters = [];
        body = [];
        super_arguments = [];
      };
      methods = [];
    }] in
  let table = create_classtable classes in
  begin
    assert_raises
      (Type_error (Lexing.dummy_pos, "cyclic inheritance involving A"))
      (fun _ -> check_class_super table "A");
    assert_raises
      (Type_error (Lexing.dummy_pos, "cyclic inheritance involving B"))
      (fun _ -> check_class_super table "B");
    assert_equal (check_class_super table "C") ();
    assert_equal (check_class_super table "D") ()
  end

let test_check_field test_cxt =
  let type_b = Type.make "B" in
  let class_a = {
    Class.name = Id.make "A";
    super = "Object";
    fields = [
      {
        Field.name = Id.make "b1";
        ty = type_b
      };
      {
        Field.name = Id.make "b2";
        ty = type_b
      }
    ];
    constructor = {
      Constructor.name = Id.make "A";
      parameters = [];
      body = [];
      super_arguments = [];
    };
    methods = [];
  } in
  let env = Environment.empty in
  let fields = Class.fields class_a in
  let f1 = List.hd fields in
  begin
    (* 空の環境にフィールド b1 を追加 *)
    assert_equal (check_field env class_a f1) ();
    let env = Environment.add (Field.name f1) (Field.ty f1) env in
    let f2 = List.nth fields 1 in
    begin
      (* b1 のみの環境にフィールド b2 を追加 *)
      assert_equal (check_field env class_a f2) ();
      (* b1 のみの環境にフィールド b1 を追加 *)
      assert_raises
        (Type_error (Lexing.dummy_pos, "variable b1 is already defined in class A"))
        (fun _ -> (check_field env class_a f1))
    end
  end

let test_check_fields test_cxt =
  let type_b = Type.make "B" in
  let class_a = {
    Class.name = Id.make "A";
    super = "Object";
    fields = [
      {
        Field.name = Id.make "b1";
        ty = type_b
      };
      {
        Field.name = Id.make "b2";
        ty = type_b
      }
    ];
    constructor = {
      Constructor.name = Id.make "A";
      parameters = [];
      body = [];
      super_arguments = [];
    };
    methods = [];
  } in
  let class_c = {
    Class.name = Id.make "C";
    super = "Object";
    fields = [
      {
        Field.name = Id.make "b1";
        ty = type_b
      };
      {
        Field.name = Id.make "b1";
        ty = type_b
      }
    ];
    constructor = {
      Constructor.name = Id.make "C";
      parameters = [];
      body = [];
      super_arguments = [];
    };
    methods = [];
  } in
  let env = check_fields Environment.empty class_a in
  begin
    assert_equal (Environment.find "b1" env) type_b;
    assert_equal (Environment.find "b2" env) type_b;
    assert_raises
      (Type_error (Lexing.dummy_pos, "variable b1 is already defined in class C"))
      (fun _ -> (check_fields Environment.empty class_c))
  end

let suite =
  "typing">::: [
    "test_create_classtable">:: test_create_classtable;
    "test_is_subclass">:: test_is_subclass;
    "test_is_subclasses">:: test_is_subclasses;
    "test_get_field">:: test_get_field;
    "test_get_method">:: test_get_method;
    "test_check_class_super">:: test_check_class_super;
    "test_check_field">:: test_check_field;
    "test_check_fields">:: test_check_fields]

let () = run_test_tt_main suite
