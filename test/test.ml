open Archived.Expression
open Archived.Interpreter

let eval1 () =
  Alcotest.(check (option int)) "eval" (Some 2) (eval (Plus ((Num 1), (Num 1))))
let eval2 () = 
  Alcotest.(check (option int)) "eval" (Some 3) (eval (Plus (Plus ((Num 1), (Num 1)), (Num 1))))
let evalminus () = 
  Alcotest.(check (option int)) "eval" (Some 1) (eval (Minus ((Num 2), (Num 1))))
let evalminusneg () = 
  Alcotest.(check (option int)) "eval" (None) (eval (Minus ((Num 1), (Num 2))))
let evalmult () = 
  Alcotest.(check (option int)) "eval" (Some 6) (eval (Times ((Num 3), (Num 2))))
let evaldiv () = 
  Alcotest.(check (option int)) "eval" (Some 4) (eval (Divide ((Num 12), (Num 3))))
let evalnoddiv () = 
  Alcotest.(check (option int)) "eval" (None) (eval (Divide ((Num 12), (Num 5))))
  let evaldivzero () = 
    Alcotest.(check (option int)) "eval" (None) (eval (Divide ((Num 12), (Num 0))))

(* Run it *)
let () =
  Alcotest.run "Eval" 
    [
      ( "add", 
        [
          Alcotest.test_case "add"  `Quick eval1;
          Alcotest.test_case "add2" `Quick eval2;
        ] );
      ( "minus",
        [
          Alcotest.test_case "m"  `Quick evalminus;
          Alcotest.test_case "mn" `Quick evalminusneg;
        ] );
      ( "mult",
        [
          Alcotest.test_case "m"  `Quick evalmult;
        ] );
      ( "div", 
        [
          Alcotest.test_case "d"  `Quick evaldiv;
          Alcotest.test_case "dn" `Quick evalnoddiv;
          Alcotest.test_case "dz" `Quick evaldivzero;
        ] );
    ]
