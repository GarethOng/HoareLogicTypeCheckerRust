open OUnit2
open Ast

(* ======== Test Utilities ======== *)

(** Helper function to parse a string *)
let parse_string s =
  let lexbuf = Lexing.from_string s in
  try Parser.program Lexer.token lexbuf with
  | Parser.Error ->
      failwith
        ("Parser error at "
        ^ string_of_int (Lexing.lexeme_start lexbuf)
        ^ ": Unexpected token '" ^ Lexing.lexeme lexbuf ^ "'")
  | Lexer.LexError msg -> failwith ("Lexer error: " ^ msg)

(** Helper function to compare types *)
let rec type_equal t1 t2 =
  match (t1, t2) with
  | TInt, TInt -> true
  | TBool, TBool -> true
  | TRef t1', TRef t2' -> type_equal t1' t2'
  | TRefMut t1', TRefMut t2' -> type_equal t1' t2'
  | TBox t1', TBox t2' -> type_equal t1' t2'
  | _, _ -> false

(** Helper for testing variable declarations *)
let assert_var_decl prog name mut type_val expr_desc expr_type =
  match prog with
  | [ Let (m, n, t, e) ] ->
      assert_equal m mut;
      assert_equal n name;
      assert (type_equal t type_val);
      assert_equal e.expr_desc expr_desc;
      assert (type_equal e.expr_type expr_type)
  | _ -> assert_failure ("Failed to parse " ^ name ^ " declaration")

(* ======== Test Groups ======== *)

(** Basic declaration tests *)
let declaration_tests =
  "declaration_tests"
  >::: [
         ( "test_int_declaration" >:: fun _ ->
           let prog = parse_string "let x: i32 = 42;" in
           assert_var_decl prog "x" false TInt (Int 42) TInt );
         ( "test_bool_declaration" >:: fun _ ->
           let prog = parse_string "let y: bool = true;" in
           assert_var_decl prog "y" false TBool (Bool true) TBool );
       ]

(** Reference tests *)
let reference_tests =
  "reference_tests"
  >::: [
         ( "test_immutable_ref" >:: fun _ ->
           let prog = parse_string "let w: &i32 = &x;" in
           match prog with
           | [ Let (false, "w", TRef TInt, expr) ] -> (
               match expr.expr_desc with
               | Unop (Ref, { expr_desc = Var "x"; _ }) -> ()
               | _ -> assert_failure "Failed to parse reference operation")
           | _ ->
               assert_failure "Failed to parse immutable reference declaration"
         );
         ( "test_mutable_ref" >:: fun _ ->
           let prog = parse_string "let mut z: &mut i32 = &mut x;" in
           match prog with
           | [ Let (true, "z", TRefMut TInt, expr) ] -> (
               match expr.expr_desc with
               | Unop (RefMut, { expr_desc = Var "x"; _ }) -> ()
               | _ -> assert_failure "Failed to parse reference operation")
           | _ -> assert_failure "Failed to parse mutable reference declaration"
         );
         ( "test_dereference" >:: fun _ ->
           let prog = parse_string "let v: i32 = *p;" in
           match prog with
           | [ Let (false, "v", TInt, expr) ] -> (
               match expr.expr_desc with
               | Unop (Deref, { expr_desc = Var "p"; _ }) -> ()
               | _ -> assert_failure "Failed to parse dereference operation")
           | _ -> assert_failure "Failed to parse dereference" );
       ]

(** Box tests *)
let box_tests =
  "box_tests"
  >::: [
         ( "test_box_int_declaration" >:: fun _ ->
           let prog = parse_string "let mut z: Box<i32> = box 42;" in
           match prog with
           | [ Let (true, "z", TBox TInt, expr) ] -> (
               match expr.expr_desc with
               | Unop (Box, { expr_desc = Int 42; expr_type = TInt }) -> ()
               | _ -> assert_failure "Failed to parse box operation")
           | _ -> assert_failure "Failed to parse box int declaration" );
         ( "test_box_bool_declaration" >:: fun _ ->
           let prog = parse_string "let mut z: Box<bool> = box true;" in
           match prog with
           | [ Let (true, "z", TBox TBool, expr) ] -> (
               match expr.expr_desc with
               | Unop (Box, { expr_desc = Bool true; expr_type = TBool }) -> ()
               | _ -> assert_failure "Failed to parse box operation")
           | _ -> assert_failure "Failed to parse box bool declaration" );
         ( "test_box_immutable_int_ref_declaration" >:: fun _ ->
           let prog = parse_string "let mut z: Box<&i32> = box &x;" in
           match prog with
           | [ Let (true, "z", TBox (TRef TInt), expr) ] -> (
               match expr.expr_desc with
               | Unop
                   ( Box,
                     { expr_desc = Unop (Ref, { expr_desc = Var "x"; _ }); _ }
                   ) ->
                   ()
               | _ ->
                   assert_failure
                     "Failed to parse reference operation nested in box")
           | _ ->
               assert_failure
                 "Failed to parse box immutable reference int declaration" );
         ( "test_box_mutable_int_ref_declaration" >:: fun _ ->
           let prog = parse_string "let mut z: Box<&mut i32> = box &mut x;" in
           match prog with
           | [ Let (true, "z", TBox (TRefMut TInt), expr) ] -> (
               match expr.expr_desc with
               | Unop
                   ( Box,
                     {
                       expr_desc = Unop (RefMut, { expr_desc = Var "x"; _ });
                       _;
                     } ) ->
                   ()
               | _ ->
                   assert_failure
                     "Failed to parse reference operation nested in box")
           | _ ->
               assert_failure
                 "Failed to parse box mutable reference int declaration" );
       ]

(** Operation tests *)
let operation_tests =
  "operation_tests"
  >::: [
         ( "test_arithmetic" >:: fun _ ->
           let prog = parse_string "let a: i32 = 10 + 5;" in
           match prog with
           | [ Let (false, "a", TInt, expr) ] -> (
               match expr.expr_desc with
               | Binop (Add, { expr_desc = Int 10; _ }, { expr_desc = Int 5; _ })
                 ->
                   ()
               | _ -> assert_failure "Failed to parse arithmetic operation")
           | _ -> assert_failure "Failed to parse arithmetic declaration" );
         ( "test_logical" >:: fun _ ->
           let prog = parse_string "let b: bool = true && false;" in
           match prog with
           | [ Let (false, "b", TBool, expr) ] -> (
               match expr.expr_desc with
               | Binop
                   ( And,
                     { expr_desc = Bool true; _ },
                     { expr_desc = Bool false; _ } ) ->
                   ()
               | _ -> assert_failure "Failed to parse logical operation")
           | _ -> assert_failure "Failed to parse logical declaration" );
       ]

(** Block scope tests *)
let block_tests =
  "block_tests"
  >::: [
         ( "test_block_scope" >:: fun _ ->
           let prog = parse_string "{ let x: i32 = 1; let y: i32 = 2; }" in
           match prog with
           | [ Block stmts ] -> assert_equal (List.length stmts) 2
           | _ -> assert_failure "Failed to parse block" );
         ( "test_immutable_ref_block_scoping" >:: fun _ ->
           let prog = parse_string "{ let x: i32 = 1; let y: &i32 = &x; }" in
           match prog with
           | [ Block stmts ] -> assert_equal (List.length stmts) 2
           | _ -> assert_failure "Failed to parse immutable reference block" );
         ( "test_mutable_ref_block_scoping" >:: fun _ ->
           let prog =
             parse_string
               "{ let mut x: i32 = 1; let mut y: &mut i32 = &mut x; }"
           in
           match prog with
           | [ Block stmts ] -> assert_equal (List.length stmts) 2
           | _ -> assert_failure "Failed to parse mutable reference block" );
       ]

(** Error case tests *)
let error_tests =
  "error_tests"
  >::: [
         ( "test_missing_type_annotation" >:: fun _ ->
           assert_raises (Failure "Parser error at 6: Unexpected token '='")
             (fun () -> parse_string "let x = 42;") );
         ( "test_invalid_reference" >:: fun _ ->
           assert_raises (Failure "Parser error at 7: Unexpected token '&&'")
             (fun () -> parse_string "let x: &&i32 = &&y;") );
       ]

(* ======== Main Test Suite ======== *)

let frontend_test_suite =
  "frontend_tests"
  >::: [
         declaration_tests;
         reference_tests;
         box_tests;
         operation_tests;
         block_tests;
         error_tests;
       ]

(* Run all the tests *)
let () = run_test_tt_main frontend_test_suite
