open OUnit2

open Syntax
open Typing

let test1 test_ctxt = assert_equal "x" "x"

let test2 test_ctxt = assert_equal 100 (1 + 99)

let test_create_classtable test_cxt =
  let classes = [
    {
      Class.name = "A";
      super = "object";
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

let suite =
  "typing">::: [
    "test_create_classtable">:: test_create_classtable;
    "test_check_class_super">:: test_check_class_super;
    "test1">:: test1;
    "test2">:: test2]

let () = run_test_tt_main suite
