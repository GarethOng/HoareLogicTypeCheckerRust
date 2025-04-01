open OUnit2
open Ast
open Verifier.Environment

let setup_test_env () =
  let env = Environment.create () in
  let lt1 = Environment.fresh_lifetime env in
  let lt2 = Environment.fresh_lifetime env in

  Environment.fresh_variable env "x" TInt lt1 false;
  Environment.fresh_variable env "y" TBool lt1 true;
  Environment.fresh_variable env "z" (TRef TInt) lt2 false;

  (env, lt1, lt2)

let environment_tests =
  "environment_tests"
  >::: [
         ( "test_create" >:: fun _ ->
           let env = Environment.create () in
           let ctx = Environment.get_lifetime_context env in

           match Environment.get_all_bindings env with
           | [] -> ()
           | _ ->
               assert_equal 0 ctx.next_id;
               assert_equal [] ctx.alive;
               assert_equal [] ctx.outlives );
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
         ( "test_variable_binding" >:: fun _ ->
           let env = Environment.create () in
           let lt = Environment.fresh_lifetime env in

           Environment.fresh_variable env "x" TInt lt false;
           assert_bool "Variable should exist" (Environment.exists env "x");

           match Environment.lookup env "x" with
           | Some binding ->
               assert_equal TInt binding.var_type;
               assert_equal lt binding.lifetime;
               assert_equal false binding.mutable_var
           | None ->
               assert_equal (Some TInt) (Environment.type_of env "x");
               assert_equal false (Environment.is_mutable env "x");
               assert_equal (Some lt) (Environment.lifetime_of env "x");

               assert_bool "Non-existent variable"
                 (not (Environment.exists env "nonexistent"));
               assert_equal None (Environment.lookup env "nonexistent") );
         ( "test_outlives" >:: fun _ ->
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
         ( "test_copy" >:: fun _ ->
           let env, lt1, lt2 = setup_test_env () in

           Environment.add_outlives env lt1 lt2;

           let copy_env = Environment.copy env in

           assert_bool "x exists in copy" (Environment.exists copy_env "x");
           assert_bool "y exists in copy" (Environment.exists copy_env "y");
           assert_bool "z exists in copy" (Environment.exists copy_env "z");

           assert_equal (Some TInt) (Environment.type_of copy_env "x");
           assert_equal (Some TBool) (Environment.type_of copy_env "y");
           assert_equal (Some (TRef TInt)) (Environment.type_of copy_env "z");

           assert_bool "lt1 outlives lt2 in copy"
             (Environment.outlives copy_env lt1 lt2);

           let lt3 = Environment.fresh_lifetime copy_env in
           Environment.fresh_variable copy_env "new_var" TInt lt3 true;

           assert_bool "new_var exists in copy"
             (Environment.exists copy_env "new_var");
           assert_bool "new_var doesn't exist in original"
             (not (Environment.exists env "new_var")) );
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

let () = run_test_tt_main environment_tests
