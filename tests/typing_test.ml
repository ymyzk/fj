open OUnit2

open Syntax
open Typing

let test_create_classtable test_cxt =
  let classes = [
    {
      Class.name = "A";
      super = "Object";
      fields = [];
      constructor = {
        Constructor.name = "A";
        parameters = [];
        body = [];
        super_arguments = [];
      };
      methods = [];
    };
    {
      Class.name = "B";
      super = "A";
      fields = [];
      constructor = {
        Constructor.name = "B";
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
      Class.name = "A";
      super = "Object";
      fields = [];
      constructor = {
        Constructor.name = "A";
        parameters = [];
        body = [];
        super_arguments = [];
      };
      methods = [];
    };
    {
      Class.name = "B";
      super = "A";
      fields = [];
      constructor = {
        Constructor.name = "B";
        parameters = [];
        body = [];
        super_arguments = [];
      };
      methods = [];
    };
    {
      Class.name = "C";
      super = "B";
      fields = [];
      constructor = {
        Constructor.name = "C";
        parameters = [];
        body = [];
        super_arguments = [];
      };
      methods = [];
    };
    {
      Class.name = "D";
      super = "A";
      fields = [];
      constructor = {
        Constructor.name = "D";
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
      Class.name = "A";
      super = "Object";
      fields = [];
      constructor = {
        Constructor.name = "A";
        parameters = [];
        body = [];
        super_arguments = [];
      };
      methods = [];
    };
    {
      Class.name = "B";
      super = "A";
      fields = [];
      constructor = {
        Constructor.name = "B";
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

let test_check_class_super test_cxt =
  let classes = [
    {
      Class.name = "A";
      super = "A";
      fields = [];
      constructor = {
        Constructor.name = "A";
        parameters = [];
        body = [];
        super_arguments = [];
      };
      methods = [];
    };
    {
      Class.name = "B";
      super = "A";
      fields = [];
      constructor = {
        Constructor.name = "B";
        parameters = [];
        body = [];
        super_arguments = [];
      };
      methods = [];
    };
    {
      Class.name = "C";
      super = "Object";
      fields = [];
      constructor = {
        Constructor.name = "C";
        parameters = [];
        body = [];
        super_arguments = [];
      };
      methods = [];
    };
    {
      Class.name = "D";
      super = "C";
      fields = [];
      constructor = {
        Constructor.name = "D";
        parameters = [];
        body = [];
        super_arguments = [];
      };
      methods = [];
    }] in
  let table = create_classtable classes in
  begin
    assert_raises
      (Type_error "cyclic inheritance involving A")
      (fun _ -> check_class_super table "A");
    assert_raises
      (Type_error "cyclic inheritance involving B")
      (fun _ -> check_class_super table "B");
    assert_equal (check_class_super table "C") ();
    assert_equal (check_class_super table "D") ()
  end

let test_check_field test_cxt =
  let type_b = Type.make "B" in
  let class_a = {
    Class.name = "A";
    super = "Object";
    fields = [
      {
        Field.name = "b1";
        ty = type_b
      };
      {
        Field.name = "b2";
        ty = type_b
      }
    ];
    constructor = {
      Constructor.name = "A";
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
        (Type_error "variable b1 is already defined in class A")
        (fun _ -> (check_field env class_a f1))
    end
  end

let test_check_fields test_cxt =
  let type_b = Type.make "B" in
  let class_a = {
    Class.name = "A";
    super = "Object";
    fields = [
      {
        Field.name = "b1";
        ty = type_b
      };
      {
        Field.name = "b2";
        ty = type_b
      }
    ];
    constructor = {
      Constructor.name = "A";
      parameters = [];
      body = [];
      super_arguments = [];
    };
    methods = [];
  } in
  let class_c = {
    Class.name = "C";
    super = "Object";
    fields = [
      {
        Field.name = "b1";
        ty = type_b
      };
      {
        Field.name = "b1";
        ty = type_b
      }
    ];
    constructor = {
      Constructor.name = "C";
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
      (Type_error "variable b1 is already defined in class C")
      (fun _ -> (check_fields Environment.empty class_c))
  end

let suite =
  "typing">::: [
    "test_create_classtable">:: test_create_classtable;
    "test_is_subclass">:: test_is_subclass;
    "test_is_subclasses">:: test_is_subclasses;
    "test_check_class_super">:: test_check_class_super;
    "test_check_field">:: test_check_field;
    "test_check_fields">:: test_check_fields]

let () = run_test_tt_main suite
