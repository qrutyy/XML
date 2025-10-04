(** Copyright 2024, Mikhail Gavrilenko, Danila Rudnev-Stepanyan*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast
open Common.Parser

(* open Angstrom *)
let test_program str = print_endline (show_program (parse_str str))

let%expect_test "factorial" =
  test_program {|let rec fact n = if n = 0 then 1 else n * fact(n-1);;|};
  [%expect
    {|
    [(Str_value (Recursive,
        ({ pat = (Pat_var "fact");
           expr =
           (Exp_fun (((Pat_var "n"), []),
              (Exp_if (
                 (Exp_apply ((Exp_ident "="),
                    (Exp_tuple
                       ((Exp_ident "n"), (Exp_constant (Const_integer 0)), []))
                    )),
                 (Exp_constant (Const_integer 1)),
                 (Some (Exp_apply ((Exp_ident "*"),
                          (Exp_tuple
                             ((Exp_ident "n"),
                              (Exp_apply ((Exp_ident "fact"),
                                 (Exp_apply ((Exp_ident "-"),
                                    (Exp_tuple
                                       ((Exp_ident "n"),
                                        (Exp_constant (Const_integer 1)),
                                        []))
                                    ))
                                 )),
                              []))
                          )))
                 ))
              ))
           },
         [])
        ))
      ]
    |}]
;;

let%expect_test "factorial2" =
  test_program {|
let rec fac n = if n <= 1 then 1 else n * fac (n - 1)

let main =
  let () = print_int (fac 4) in
  0
;;
|};
    [%expect {|
      [(Str_value (Recursive,
          ({ pat = (Pat_var "fac");
             expr =
             (Exp_fun (((Pat_var "n"), []),
                (Exp_if (
                   (Exp_apply ((Exp_ident "<="),
                      (Exp_tuple
                         ((Exp_ident "n"), (Exp_constant (Const_integer 1)), []))
                      )),
                   (Exp_constant (Const_integer 1)),
                   (Some (Exp_apply ((Exp_ident "*"),
                            (Exp_tuple
                               ((Exp_ident "n"),
                                (Exp_apply ((Exp_ident "fac"),
                                   (Exp_apply ((Exp_ident "-"),
                                      (Exp_tuple
                                         ((Exp_ident "n"),
                                          (Exp_constant (Const_integer 1)),
                                          []))
                                      ))
                                   )),
                                []))
                            )))
                   ))
                ))
             },
           [])
          ));
        (Str_value (Nonrecursive,
           ({ pat = (Pat_var "main");
              expr =
              (Exp_let (Nonrecursive,
                 ({ pat = (Pat_construct ("()", None));
                    expr =
                    (Exp_apply ((Exp_ident "print_int"),
                       (Exp_apply ((Exp_ident "fac"),
                          (Exp_constant (Const_integer 4))))
                       ))
                    },
                  []),
                 (Exp_constant (Const_integer 0))))
              },
            [])
           ))
        ]
      |}]
;;

