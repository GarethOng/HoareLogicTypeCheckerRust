open OUnit2
open Ast

(* Helper function to parse a string *)
let parse_string s =
  let lexbuf = Lexing.from_string s in
  try Parser.program Lexer.token lexbuf with
  | Parser.Error ->
      failwith
        ("Parser error at "
        ^ string_of_int (Lexing.lexeme_start lexbuf)
        ^ ": Unexpected token '" ^ Lexing.lexeme lexbuf ^ "'")
  | Lexer.LexError msg -> failwith ("Lexer error: " ^ msg)

(* Helper function to compare types *)
let rec type_equal t1 t2 =
  match (t1, t2) with
  | TInt, TInt -> true
  | TBool, TBool -> true
  | TRef t1', TRef t2' -> type_equal t1' t2'
  | TRefMut t1', TRefMut t2' -> type_equal t1' t2'
  | _, _ -> false

(* Test cases *)
let tests =
  "test_frontend"
  >::: [
         (* Test basic integer declaration *)
         ( "test_int_declaration" >:: fun _ ->
           let prog = parse_string "let x: i32 = 42;" in
           match prog with
           | [ Let (false, "x", TInt, expr) ] ->
               assert_equal expr.expr_desc (Int 42);
               assert (type_equal expr.expr_type TInt)
           | _ -> assert_failure "Failed to parse integer declaration" );
         (* Test boolean declaration *)
         ( "test_bool_declaration" >:: fun _ ->
           let prog = parse_string "let y: bool = true;" in
           match prog with
           | [ Let (false, "y", TBool, expr) ] ->
               assert_equal expr.expr_desc (Bool true);
               assert (type_equal expr.expr_type TBool)
           | _ -> assert_failure "Failed to parse boolean declaration" );
         (* Test mutable reference *)
         ( "test_mutable_ref" >:: fun _ ->
           let prog = parse_string "let mut z: &mut i32 = &mut x;" in
           match prog with
           | [ Let (true, "z", TRefMut TInt, expr) ] -> (
               match expr.expr_desc with
               | Unop (RefMut, { expr_desc = Var "x"; expr_type = _; _ }) -> ()
               | _ -> assert_failure "Failed to parse reference operation")
           | _ -> assert_failure "Failed to parse mutable reference declaration"
         );
         (* Test immutable reference *)
         ( "test_immutable_ref" >:: fun _ ->
           let prog = parse_string "let w: &i32 = &x;" in
           match prog with
           | [ Let (false, "w", TRef TInt, expr) ] -> (
               match expr.expr_desc with
               | Unop (Ref, { expr_desc = Var "x"; expr_type = _; _ }) -> ()
               | _ -> assert_failure "Failed to parse reference operation")
           | _ ->
               assert_failure "Failed to parse immutable reference declaration"
         );
         (* Test arithmetic operations *)
         ( "test_arithmetic" >:: fun _ ->
           let prog = parse_string "let a: i32 = 10 + 5;" in
           match prog with
           | [ Let (false, "a", TInt, expr) ] -> (
               match expr.expr_desc with
               | Binop
                   ( Add,
                     { expr_desc = Int 10; expr_type = _; _ },
                     { expr_desc = Int 5; expr_type = _; _ } ) ->
                   ()
               | _ -> assert_failure "Failed to parse arithmetic operation")
           | _ -> assert_failure "Failed to parse arithmetic declaration" );
         (* Test logical operations *)
         ( "test_logical" >:: fun _ ->
           let prog = parse_string "let b: bool = true && false;" in
           match prog with
           | [ Let (false, "b", TBool, expr) ] -> (
               match expr.expr_desc with
               | Binop
                   ( And,
                     { expr_desc = Bool true; expr_type = _; _ },
                     { expr_desc = Bool false; expr_type = _; _ } ) ->
                   ()
               | _ -> assert_failure "Failed to parse logical operation")
           | _ -> assert_failure "Failed to parse logical declaration" );
         (* Test block scoping *)
         ( "test_block_scope" >:: fun _ ->
           let prog = parse_string "{ let x: i32 = 1; let y: i32 = 2; }" in
           match prog with
           | [ Block stmts ] -> assert_equal (List.length stmts) 2
           | _ -> assert_failure "Failed to parse block" );
         (* Test dereferencing *)
         ( "test_dereference" >:: fun _ ->
           let prog = parse_string "let v: i32 = *p;" in
           match prog with
           | [ Let (false, "v", TInt, expr) ] -> (
               match expr.expr_desc with
               | Unop (Deref, { expr_desc = Var "p"; expr_type = _; _ }) -> ()
               | _ -> assert_failure "Failed to parse dereference operation")
           | _ -> assert_failure "Failed to parse dereference" );
         (* Test error cases *)
         ( "test_missing_type_annotation" >:: fun _ ->
           assert_raises (Failure "Parser error at 6: Unexpected token '='")
             (fun () -> parse_string "let x = 42;") );
         ( "test_invalid_reference" >:: fun _ ->
           assert_raises (Failure "Parser error at 7: Unexpected token '&&'")
             (fun () -> parse_string "let x: &&i32 = &&y;")
         (* Double reference not allowed *) );
       ]

(* Run the tests *)
let () = run_test_tt_main tests
