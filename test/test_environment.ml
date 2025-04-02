open OUnit2
open Ast
open Verifier.Environment

(* Helper function to set up a common test environment *)
let setup_test_env () =
  let env = Environment.create () in
  let lt1 = Environment.fresh_lifetime env in
  let lt2 = Environment.fresh_lifetime env in
  let lt3 = Environment.fresh_lifetime env in

  Environment.add_outlives env lt1 lt3;
  Environment.fresh_variable env "x" TInt lt1 false;
  Environment.fresh_variable env "y" TBool lt2 true;
  Environment.fresh_variable env "z" (TRef TInt) lt3 false;

  (env, lt1, lt2, lt3)

let basic_environment_tests =
  [
    ( "test_create" >:: fun _ ->
      let env = Environment.create () in
      let ctx = Environment.get_lifetime_context env in

      assert_equal [] (Environment.get_all_bindings env);
      assert_equal 0 ctx.next_id;
      assert_equal [] ctx.alive;
      assert_equal [] ctx.outlives );
  ]

let lifetime_tests =
  [
    ( "test_fresh_lifetime" >:: fun _ ->
      let env = Environment.create () in
      let lt1 = Environment.fresh_lifetime env in
      let lt2 = Environment.fresh_lifetime env in
      let lt3 = Environment.fresh_lifetime env in

      assert_equal "'t0" lt1;
      assert_equal "'t1" lt2;
      assert_equal "'t2" lt3;

      let ctx = Environment.get_lifetime_context env in
      assert_equal 3 ctx.next_id;
      assert_bool "lt1 in alive list" (List.mem lt1 ctx.alive);
      assert_bool "lt2 in alive list" (List.mem lt2 ctx.alive);
      assert_bool "lt3 in alive list" (List.mem lt3 ctx.alive) );
  ]

let variable_binding_tests =
  [
    ( "test_variable_binding" >:: fun _ ->
      let env = Environment.create () in
      let lt = Environment.fresh_lifetime env in

      Environment.fresh_variable env "x" TInt lt false;
      assert_bool "Variable should exist" (Environment.exists env "x");

      (match Environment.lookup env "x" with
      | Some binding ->
          assert_equal TInt binding.var_type;
          assert_equal lt binding.lifetime;
          assert_equal false binding.mutable_var
      | None -> assert_failure "Expected to find binding for x");

      assert_equal (Some TInt) (Environment.type_of env "x");
      assert_equal false (Environment.is_mutable env "x");
      assert_equal (Some lt) (Environment.lifetime_of env "x");

      assert_bool "Non-existent variable"
        (not (Environment.exists env "nonexistent"));
      assert_equal None (Environment.lookup env "nonexistent") );
  ]

let outlives_tests =
  [
    ( "test_simple_outlives" >:: fun _ ->
      let env = Environment.create () in
      let lt1 = Environment.fresh_lifetime env in
      let lt2 = Environment.fresh_lifetime env in
      let lt3 = Environment.fresh_lifetime env in

      Environment.add_outlives env lt1 lt2;
      Environment.add_outlives env lt2 lt3;

      assert_bool "lt1 outlives lt2" (Environment.outlives env lt1 lt2);
      assert_bool "lt2 outlives lt3" (Environment.outlives env lt2 lt3);

      assert_bool "lt1 outlives lt3 (transitive)"
        (Environment.outlives env lt1 lt3);

      assert_bool "lt3 doesn't outlive lt2"
        (not (Environment.outlives env lt3 lt2));
      assert_bool "lt2 doesn't outlive lt1"
        (not (Environment.outlives env lt2 lt1)) );
    ( "test_complex_lifetime_scenario" >:: fun _ ->
      let env = Environment.create () in

      let lt_a = Environment.fresh_lifetime env in
      let lt_b = Environment.fresh_lifetime env in
      let lt_c = Environment.fresh_lifetime env in
      let lt_d = Environment.fresh_lifetime env in
      let lt_e = Environment.fresh_lifetime env in

      (* Create a diamond-shaped relationship:
        a → b → d
        ↓   ↑
        c → e
      *)
      Environment.add_outlives env lt_a lt_b;
      Environment.add_outlives env lt_a lt_c;
      Environment.add_outlives env lt_b lt_d;
      Environment.add_outlives env lt_c lt_e;
      Environment.add_outlives env lt_e lt_b;

      assert_bool "a outlives b" (Environment.outlives env lt_a lt_b);
      assert_bool "a outlives c" (Environment.outlives env lt_a lt_c);
      assert_bool "b outlives d" (Environment.outlives env lt_b lt_d);
      assert_bool "c outlives e" (Environment.outlives env lt_c lt_e);
      assert_bool "e outlives b" (Environment.outlives env lt_e lt_b);

      assert_bool "a outlives d" (Environment.outlives env lt_a lt_d);
      assert_bool "a outlives e" (Environment.outlives env lt_a lt_e);
      assert_bool "c outlives b" (Environment.outlives env lt_c lt_b);
      assert_bool "c outlives d" (Environment.outlives env lt_c lt_d);

      assert_bool "d doesn't outlive a"
        (not (Environment.outlives env lt_d lt_a));
      assert_bool "b doesn't outlive a"
        (not (Environment.outlives env lt_b lt_a)) );
  ]

let environment_copy_tests =
  [
    ( "test_copy" >:: fun _ ->
      let env, lt1, lt2, lt3 = setup_test_env () in

      let copy_env = Environment.copy env in

      assert_bool "x exists in copy" (Environment.exists copy_env "x");
      assert_bool "y exists in copy" (Environment.exists copy_env "y");
      assert_bool "z exists in copy" (Environment.exists copy_env "z");

      assert_equal (Some TInt) (Environment.type_of copy_env "x");
      assert_equal (Some TBool) (Environment.type_of copy_env "y");
      assert_equal (Some (TRef TInt)) (Environment.type_of copy_env "z");

      assert_bool "lt1 outlives lt3 in copy"
        (Environment.outlives copy_env lt1 lt3);

      assert_equal false (Environment.outlives copy_env lt1 lt2);
      assert_equal false (Environment.outlives copy_env lt2 lt1);
      assert_equal false (Environment.outlives copy_env lt3 lt2);
      assert_equal false (Environment.outlives copy_env lt2 lt3);

      let lt4 = Environment.fresh_lifetime copy_env in
      Environment.fresh_variable copy_env "new_var" TInt lt4 true;

      assert_bool "new_var exists in copy"
        (Environment.exists copy_env "new_var");
      assert_bool "new_var doesn't exist in original"
        (not (Environment.exists env "new_var")) );
  ]

let read_write_prohibited_test =
  [
    ( "test_immutable_borrowed" >:: fun _ ->
      let env, lt1, _, lt3 = setup_test_env () in

      assert_equal (Some TInt) (Environment.type_of env "x");
      assert_equal (Some TBool) (Environment.type_of env "y");
      assert_equal (Some (TRef TInt)) (Environment.type_of env "z");

      assert_bool "lt1 outlives lt3 in copy" (Environment.outlives env lt1 lt3);

      assert_bool "lt1 is write prohibited"
        (Environment.write_prohibited env lt1);
      assert_bool "lt3 is write prohibited"
        (not (Environment.write_prohibited env lt3));
      assert_bool "lt1 is not read prohibited"
        (not (Environment.read_prohibited env lt1));
      assert_bool "lt3 is not read prohibited"
        (not (Environment.read_prohibited env lt3)) );
    ( "test_mutable_borrowed" >:: fun _ ->
      let env = Environment.create () in
      let lt1 = Environment.fresh_lifetime env in
      let lt2 = Environment.fresh_lifetime env in

      Environment.fresh_variable env "x" TInt lt1 true;
      assert_bool "Variable should exist" (Environment.exists env "x");
      Environment.fresh_variable env "y" (TRefMut TInt) lt2 false;
      assert_bool "Variable should exist" (Environment.exists env "y");

      Environment.add_outlives env lt1 lt2;
      assert_bool "lt1 outlives lt2 in copy" (Environment.outlives env lt1 lt2);

      assert_bool "lt1 is write prohibited"
        (Environment.write_prohibited env lt1);
      assert_bool "lt2 is not write prohibited"
        (not (Environment.write_prohibited env lt2));

      assert_bool "lt1 is read prohibited" (Environment.read_prohibited env lt1);
      assert_bool "lt2 is not read prohibited"
        (not (Environment.read_prohibited env lt2)) );
    ( "test_multiple_immutable_borrows" >:: fun _ ->
      let env = Environment.create () in
      let lt1 = Environment.fresh_lifetime env in
      let lt2 = Environment.fresh_lifetime env in
      let lt3 = Environment.fresh_lifetime env in
      let lt4 = Environment.fresh_lifetime env in

      Environment.fresh_variable env "x" TInt lt1 true;
      assert_bool "Variable should exist" (Environment.exists env "x");
      Environment.fresh_variable env "y1" (TRef TInt) lt2 false;
      assert_bool "Variable should exist" (Environment.exists env "y1");
      Environment.fresh_variable env "y2" (TRef TInt) lt3 false;
      assert_bool "Variable should exist" (Environment.exists env "y2");
      Environment.fresh_variable env "z1" (TRef (TRef TInt)) lt4 false;
      assert_bool "Variable should exist" (Environment.exists env "z1");

      Environment.add_outlives env lt1 lt2;
      assert_bool "lt1 outlives lt2 in copy" (Environment.outlives env lt1 lt2);
      Environment.add_outlives env lt1 lt3;
      assert_bool "lt1 outlives lt3 in copy" (Environment.outlives env lt1 lt3);
      Environment.add_outlives env lt3 lt4;
      assert_bool "lt3 outlives lt4 in copy" (Environment.outlives env lt3 lt4);

      assert_bool "lt1 is write prohibited"
        (Environment.write_prohibited env lt1);

      assert_bool "lt1 is not read prohibited"
        (not (Environment.read_prohibited env lt1));
      assert_bool "lt2 is not read prohibited"
        (not (Environment.read_prohibited env lt2));
      assert_bool "lt3 is not read prohibited"
        (not (Environment.read_prohibited env lt3));
      assert_bool "lt4 is not read prohibited"
        (not (Environment.read_prohibited env lt4)) );
    ( "test_multiple_mutable_borrows" >:: fun _ ->
      let env = Environment.create () in
      let lt1 = Environment.fresh_lifetime env in
      let lt2 = Environment.fresh_lifetime env in
      let lt3 = Environment.fresh_lifetime env in
      let lt4 = Environment.fresh_lifetime env in

      Environment.fresh_variable env "x" TInt lt1 true;
      assert_bool "Variable should exist" (Environment.exists env "x");
      Environment.fresh_variable env "y1" (TRefMut TInt) lt2 false;
      assert_bool "Variable should exist" (Environment.exists env "y1");
      Environment.fresh_variable env "y2" (TRefMut TInt) lt3 false;
      assert_bool "Variable should exist" (Environment.exists env "y2");
      Environment.fresh_variable env "z1" (TRefMut (TRefMut TInt)) lt4 false;
      assert_bool "Variable should exist" (Environment.exists env "z1");

      (*Before borrowing*)
      assert_bool "lt1 is not read prohibited"
        (not (Environment.read_prohibited env lt1));
      assert_bool "lt1 is not write prohibited"
        (not (Environment.write_prohibited env lt1));

      (*y1 borrows from x*)
      Environment.add_outlives env lt1 lt2;
      assert_bool "lt1 outlives lt2 in copy" (Environment.outlives env lt1 lt2);

      (*After y1 borrows from x*)
      assert_bool "lt1 is read prohibited" (Environment.read_prohibited env lt1);
      assert_bool "lt1 is write prohibited"
        (Environment.write_prohibited env lt1);
      assert_bool "lt2 is not read prohibited"
        (not (Environment.read_prohibited env lt2));
      assert_bool "lt2 is not write prohibited"
        (not (Environment.write_prohibited env lt2));

      (*y2 borrows from y1*)
      Environment.add_outlives env lt2 lt3;
      assert_bool "lt2 outlives lt3 in copy" (Environment.outlives env lt2 lt3);

      (*After y2 borrows from y1*)
      assert_bool "lt1 is read prohibited" (Environment.read_prohibited env lt1);
      assert_bool "lt1 is write prohibited"
        (Environment.write_prohibited env lt1);
      assert_bool "lt2 is read prohibited" (Environment.read_prohibited env lt2);
      assert_bool "lt2 is write prohibited"
        (Environment.write_prohibited env lt2);
      assert_bool "lt3 is not read prohibited"
        (not (Environment.read_prohibited env lt3));
      assert_bool "lt3 is not write prohibited"
        (not (Environment.write_prohibited env lt3));

      (*z1 borrows from y2*)
      Environment.add_outlives env lt3 lt4;
      assert_bool "lt3 outlives lt4 in copy" (Environment.outlives env lt3 lt4);

      (*After z1 borrows from y2*)
      assert_bool "lt1 is read prohibited" (Environment.read_prohibited env lt1);
      assert_bool "lt1 is write prohibited"
        (Environment.write_prohibited env lt1);
      assert_bool "lt2 is read prohibited" (Environment.read_prohibited env lt2);
      assert_bool "lt2 is write prohibited"
        (Environment.write_prohibited env lt2);
      assert_bool "lt3 is read prohibited" (Environment.read_prohibited env lt3);
      assert_bool "lt3 is write prohibited"
        (Environment.write_prohibited env lt3);
      assert_bool "lt4 is not read prohibited"
        (not (Environment.read_prohibited env lt4));
      assert_bool "lt4 is not write prohibited"
        (not (Environment.write_prohibited env lt4)) );
    ( "test_multiple_mutable_borrows" >:: fun _ ->
      let env = Environment.create () in
      let lt1 = Environment.fresh_lifetime env in
      let lt2 = Environment.fresh_lifetime env in
      let lt3 = Environment.fresh_lifetime env in
      let lt4 = Environment.fresh_lifetime env in

      Environment.fresh_variable env "x" TInt lt1 true;
      assert_bool "Variable should exist" (Environment.exists env "x");
      Environment.fresh_variable env "y1" (TRefMut TInt) lt2 false;
      assert_bool "Variable should exist" (Environment.exists env "y1");
      Environment.fresh_variable env "y2" (TRefMut TInt) lt3 false;
      assert_bool "Variable should exist" (Environment.exists env "y2");
      Environment.fresh_variable env "z1" (TRefMut (TRefMut TInt)) lt4 false;
      assert_bool "Variable should exist" (Environment.exists env "z1");

      (*Before borrowing*)
      assert_bool "lt1 is not read prohibited"
        (not (Environment.read_prohibited env lt1));
      assert_bool "lt1 is not write prohibited"
        (not (Environment.write_prohibited env lt1));

      (*y1 borrows from x*)
      Environment.add_outlives env lt1 lt2;
      assert_bool "lt1 outlives lt2 in copy" (Environment.outlives env lt1 lt2);

      (*After y1 borrows from x*)
      assert_bool "lt1 is read prohibited" (Environment.read_prohibited env lt1);
      assert_bool "lt1 is write prohibited"
        (Environment.write_prohibited env lt1);
      assert_bool "lt2 is not read prohibited"
        (not (Environment.read_prohibited env lt2));
      assert_bool "lt2 is not write prohibited"
        (not (Environment.write_prohibited env lt2));

      (*y2 borrows from y1*)
      Environment.add_outlives env lt2 lt3;
      assert_bool "lt2 outlives lt3 in copy" (Environment.outlives env lt2 lt3);

      (*After y2 borrows from y1*)
      assert_bool "lt1 is read prohibited" (Environment.read_prohibited env lt1);
      assert_bool "lt1 is write prohibited"
        (Environment.write_prohibited env lt1);
      assert_bool "lt2 is read prohibited" (Environment.read_prohibited env lt2);
      assert_bool "lt2 is write prohibited"
        (Environment.write_prohibited env lt2);
      assert_bool "lt3 is not read prohibited"
        (not (Environment.read_prohibited env lt3));
      assert_bool "lt3 is not write prohibited"
        (not (Environment.write_prohibited env lt3));

      (*end y2's lifetime*)
      Environment.remove_lifetime env lt3;
      assert_bool "lt3 no longer alive" (not (Environment.alive env lt3));
      assert_bool "no longer valid to compare lt2 to lt3"
        (not (Environment.outlives env lt2 lt3));
      assert_bool "no longer valid to compare lt1 to lt3"
        (not (Environment.outlives env lt1 lt3));

      assert_bool "lt1 is read prohibited" (Environment.read_prohibited env lt1);
      assert_bool "lt1 is write prohibited"
        (Environment.write_prohibited env lt1);
      assert_bool "lt2 is not read prohibited"
        (not (Environment.read_prohibited env lt2));
      assert_bool "lt2 is not write prohibited"
        (not (Environment.write_prohibited env lt2));
      assert_bool "lt3 is read prohibited" (Environment.read_prohibited env lt3);
      assert_bool "lt3 is write prohibited"
        (Environment.write_prohibited env lt3);

      (*z1 borrows from y1*)
      Environment.add_outlives env lt2 lt4;
      assert_bool "lt2 outlives lt4 in copy" (Environment.outlives env lt2 lt4);

      (*After z1 borrows from y2*)
      assert_bool "lt1 is read prohibited" (Environment.read_prohibited env lt1);
      assert_bool "lt1 is write prohibited"
        (Environment.write_prohibited env lt1);
      assert_bool "lt2 is read prohibited" (Environment.read_prohibited env lt2);
      assert_bool "lt2 is write prohibited"
        (Environment.write_prohibited env lt2);
      assert_bool "lt3 is read prohibited" (Environment.read_prohibited env lt3);
      assert_bool "lt3 is write prohibited"
        (Environment.write_prohibited env lt3);
      assert_bool "lt4 is not read prohibited"
        (not (Environment.read_prohibited env lt4));
      assert_bool "lt4 is not write prohibited"
        (not (Environment.write_prohibited env lt4)) );
  ]

let environment_tests =
  "environment_tests"
  >::: List.flatten
         [
           basic_environment_tests;
           lifetime_tests;
           variable_binding_tests;
           outlives_tests;
           environment_copy_tests;
           read_write_prohibited_test;
         ]

let () = run_test_tt_main environment_tests
