(** Copyright 2024,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko*)

(** SPDX-License-(ident[@gen gen_ident])ifier: LGPL-3.0-or-later *)

open QCheck
open Base
open Gen
open Stdlib

type ident = string [@@deriving eq, show { with_path = false }]

let gen_charc = map Char.chr (int_range (Char.code 'a') (Char.code 'z'))

let is_not_keyword = function
  | "let"
  | "if"
  | "then"
  | "else"
  | "in"
  | "fun"
  | "true"
  | "false"
  | "rec"
  | "and"
  | "function"
  | "match"
  | "with"
  | "type"
  | "of" -> false
  | _ -> true
;;

let rec gen_filtered_ident base_gen =
  let open QCheck.Gen in
  base_gen
  >>= fun (ident [@gen gen_ident]) ->
  if is_not_keyword (ident [@gen gen_ident])
  then return (ident [@gen gen_ident])
  else gen_filtered_ident base_gen
;;

let gen_id_first_char = frequency [ 5, char_range 'a' 'z'; 1, return '_' ]
let gen_digit = char_range '0' '9'

let gen_id_char =
  frequency [ 5, gen_id_first_char; 5, char_range 'A' 'Z'; 5, gen_digit; 1, return '\'' ]
;;

let gen_ident =
  let gen_name =
    let* fst = gen_id_first_char >|= fun c -> Base.Char.to_string c in
    let range = if Base.String.( = ) fst "_" then 1 -- 4 else 0 -- 4 in
    let* rest = string_size ~gen:gen_id_char range in
    return (fst ^ rest)
  in
  let rec loop gen =
    gen >>= fun name -> if is_not_keyword name then return name else loop gen
  in
  loop gen_name
;;

let gen_ident_uc =
  let base_gen =
    map2
      (fun start_sym rest_sym -> Base.Char.to_string start_sym ^ rest_sym)
      (char_range 'A' 'Z')
      (string_small_of
         (oneof
            [ char_range 'A' 'Z'; char_range 'a' 'z'; char_range '0' '9'; return '_' ]))
  in
  gen_filtered_ident base_gen
;;

let gen_ident_lc include_us =
  let start_sym =
    if include_us then oneof [ char_range 'a' 'z'; return '_' ] else char_range 'a' 'z'
  in
  let base_gen =
    map2
      (fun start_sym rest_sym -> Base.Char.to_string start_sym ^ rest_sym)
      start_sym
      (string_small_of
         (oneof
            [ char_range 'A' 'Z'; char_range 'a' 'z'; char_range '0' '9'; return '_' ]))
  in
  gen_filtered_ident base_gen
;;

module List1 = struct
  type 'a t = 'a * ('a list[@gen list_size (int_bound 5) gen_a])
  [@@deriving eq, show { with_path = false }, qcheck]
end

module List2 = struct
  type 'a t = 'a * 'a * ('a list[@gen list_size (int_bound 5) gen_a])
  [@@deriving eq, show { with_path = false }, qcheck]
end

module Constant = struct
  type t =
    | Const_integer of (int[@gen nat_small]) (** integer as [52] *)
    | Const_char of (char[@gen gen_charc]) (** char as ['w'] *)
    | Const_string of (string[@gen string_small_of gen_charc]) (** string as ["Kakadu"] *)
  [@@deriving eq, show { with_path = false }, qcheck]
end

module TypeExpr = struct
  let gen_ref inner_gen =
    let open QCheck.Gen in
    map ref inner_gen
  ;;

  type level = (int[@gen nat_small]) [@@deriving eq, show { with_path = false }, qcheck]

  type t =
    | Type_arrow of (t[@gen gen_sized (n / 2)]) * (t[@gen gen_sized (n / 2)])
    (** Function type [t1 -> t2] *)
    | Type_tuple of t List2.t (** Tuple type [t1 * t2 * ...] *)
    | Type_var of tv ref (** Type variable ['a] *)
    | Quant_type_var of (ident[@gen gen_ident]) (** Quantified type variable ['a. 'a] *)
    | Type_construct of (ident[@gen gen_ident]) * t list (** *)
  [@@deriving eq, show { with_path = false }, qcheck]

  and tv =
    | Unbound of (ident[@gen gen_ident]) * level (** Free type variable *)
    | Link of (t[@gen gen_sized (n / 2)]) (** Unified type variable *)
  [@@deriving eq, show { with_path = false }, qcheck]
end

module Pattern = struct
  type t =
    | Pat_constraint of t * (TypeExpr.t[@gen TypeExpr.gen_sized (n / 2)])
    (** Pattern [(P : T)] *)
    | Pat_any (** The pattern [_]. *)
    | Pat_var of (ident[@gen gen_ident]) (** A variable pattern such as [x] *)
    | Pat_constant of Constant.t (** Patterns such as [52], ['w'], ["uwu"] *)
    | Pat_tuple of t List2.t (** Patterns [(P1, ..., Pn)]. *)
    | Pat_construct of (ident[@gen gen_ident]) * t option
    (** [Pat_construct(C, args)] represents:
        - [C]               when [args] is [None],
        - [C P]             when [args] is [Some (P)]
        - [C (P1, ..., Pn)] when [args] is
          [Some (Pat_tuple [P1; ...; Pn])] *)
  [@@deriving eq, show { with_path = false }, qcheck]
end

module Expression = struct
  type rec_flag =
    | Nonrecursive
    | Recursive
  [@@deriving eq, show { with_path = false }, qcheck]

  type 'expr value_binding =
    { pat : Pattern.t
    ; expr : 'expr
    }
  [@@deriving eq, show { with_path = false }]

  let gen_value_binding gen_expr n =
    map2 (fun pat expr -> { pat; expr }) (Pattern.gen_sized (n / 2)) (gen_expr (n / 2))
  ;;

  type 'expr case =
    { first : Pattern.t
    ; second : 'expr
    }
  [@@deriving eq, show { with_path = false }]

  let gen_case gen_expr n =
    map2
      (fun first second -> { first; second })
      (Pattern.gen_sized (n / 2))
      (gen_expr (n / 2))
  ;;

  type t =
    | Exp_ident of (ident[@gen gen_ident])
    (** (ident[@gen gen_ident])ifiers such as [x] *)
    | Exp_constant of Constant.t (** Expressions constant such as [1], ['a'], ["true"]**)
    | Exp_tuple of t List2.t (** Expressions [(E1, E2, ..., En)] *)
    | Exp_function of (t case[@gen gen_case gen_sized (n / 2)]) List1.t
    (** [Exp_function (P1, [P2; ...; Pn])] represents
        [function P1 | ... | Pn] *)
    | Exp_fun of (Pattern.t[@gen Pattern.gen_sized (n / 2)]) List1.t * t
    (**[Exp_fun (P1, [P2; ...; Pn], E)] represents:
       [fun P1 ... Pn -> E] *)
    | Exp_apply of t * t
    (** [Pexp_apply(E0, E1)]
                             represents [E0 E1]*)
    | Exp_match of t * (t case[@gen gen_case gen_sized (n / 2)]) List1.t
    (** [match E0 with P1 -> E1 || Pn -> En] *)
    | Exp_constraint of t * (TypeExpr.t[@gen TypeExpr.gen_sized (n / 2)]) (** [(E : T)] *)
    | Exp_if of t * t * t option (** [if E1 then E2 else E3] *)
    | Exp_let of
        rec_flag * (t value_binding[@gen gen_value_binding gen_sized (n / 2)]) List1.t * t
    (** [Exp_let(flag, [(P1,E1) ; ... ; (Pn,En)], E)] represents:
        - [let P1 = E1 and ... and Pn = EN in E]
          when [flag] is [Nonrecursive],
        - [let rec P1 = E1 and ... and Pn = EN in E]
          when [flag] is [Recursive]. *)
    | Exp_construct of (ident[@gen gen_ident]) * t option
    (** [Exp_construct(C, exp)] represents:
        - [C]               when [exp] is [None],
        - [C E]             when [exp] is [Some E],
        - [C (E1, ..., En)] when [exp] is [Some (Exp_tuple[E1;...;En])] *)
  [@@deriving eq, show { with_path = false }, qcheck]
end

module Structure = struct
  type structure_item =
    | Str_eval of Expression.t
    | Str_value of Expression.rec_flag * Expression.t Expression.value_binding List1.t
    (** [Str_value(rec, [(P1, E1 ; ... ; (Pn, En))])] represents:
        - [let P1 = E1 and ... and Pn = EN]
          when [rec] is [Nonrecursive],
        - [let rec P1 = E1 and ... and Pn = EN ]
          when [rec] is [Recursiv e ee]. *)
    | Str_adt of
        (ident[@gen gen_ident]) list
        * (ident[@gen gen_ident])
        * ((ident[@gen gen_ident]) * TypeExpr.t option) List1.t
    (** [Str_type(C0, [(C1, [(T11; T12; ... ; T1n_1)]); (C2, [(T21;T22; ... ; T2n_2)]); ... ;
      (Cm, [(Tm1;Tm2; ... ; Tmn_n)]) ])] represents:

        [type C0 =
      | C1 of T11 * ... * T1n_1
      | ...
      | Cm of Tm1 * ... * Tmn_n
      ]

        n_i: [n_i >= 0]
        Invariant: [m > 0] *)
  [@@deriving eq, show { with_path = false }]

  let gen_structure_item n =
    oneof_weighted
      [ 0, map (fun expr -> Str_eval expr) (Expression.gen_sized (n / 2))
      ; ( 1
        , let* rec_flag =
            oneof [ return Expression.Nonrecursive; return Expression.Recursive ]
          in
          let* bind1 = Expression.gen_value_binding Expression.gen_sized (n / 2) in
          let* bindl =
            list_small (Expression.gen_value_binding Expression.gen_sized (n / 2))
          in
          return (Str_value (rec_flag, (bind1, bindl))) )
      ; ( 0
        , let* tparam = list_small gen_ident in
          let* idt = gen_ident in
          let* cons1 = Gen.pair gen_ident (Gen.option (TypeExpr.gen_sized (n / 20))) in
          let* consl =
            list_small (Gen.pair gen_ident (Gen.option (TypeExpr.gen_sized (n / 20))))
          in
          return (Str_adt (tparam, idt, (cons1, consl))) )
      ]
  ;;
end

type program = Structure.structure_item list [@@deriving eq, show { with_path = false }]

module Program = struct
  let gen_program n = list_size (int_bound 6) (Structure.gen_structure_item (n / 2))
end
