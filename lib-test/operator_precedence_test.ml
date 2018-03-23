(*
 * Copyright (c) 2017-2018 Takahisa Watanabe <linerlock@outlook.com> All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *)
open OUnit2
open Core
open Core.Option
open Finale
open Finale.Syntax
open Finale.Iso
open Finale.Iso_partial

module Make (Pretty: Pretty_intf.S) (Parser: Parser_intf.S) = struct
  module Combinator_base = Syntax.Make (Pretty) (Parser)
  module Combinator = Combinator.Make (Combinator_base)
  module Operator_precedence = Operator_precedence.Make (Combinator_base)
  
  open Combinator_base
  open Combinator
  open Operator_precedence

  type 'a exp =
    | AddE: int exp * int exp -> int exp
    | SubE: int exp * int exp -> int exp
    | MulE: int exp * int exp -> int exp
    | DivE: int exp * int exp -> int exp
    | IntE: int -> int exp
    | IncE: int exp -> int exp
    | DecE: int exp -> int exp
        
  let addE =
    { fwd = (function (e0, e1) -> Some (AddE (e0, e1)));
      bwd = (function AddE (e0, e1) -> Some (e0, e1) | _ -> None)
    }

  let subE =
    { fwd = (function (e0, e1) -> Some (SubE (e0, e1)));
      bwd = (function SubE (e0, e1) -> Some (e0, e1) | _ -> None)
    }

  let mulE =
    { fwd = (function (e0, e1) -> Some (MulE (e0, e1)));
      bwd = (function MulE (e0, e1) -> Some (e0, e1) | _ -> None)
    }

  let divE =
    { fwd = (function (e0, e1) -> Some (DivE (e0, e1)));
      bwd = (function DivE (e0, e1) -> Some (e0, e1) | _ -> None)
    }

  let incE =
    { fwd = (function e0 -> Some (IncE e0));
      bwd = (function IncE e0 -> Some e0 | _ -> None)
    }

  let decE =
    { fwd = (function e0 -> Some (DecE e0));
      bwd = (function DecE e0 -> Some e0 | _ -> None)
    }

  let intE =
    { fwd = (function n0 -> Some (IntE n0));
      bwd = (function IntE n0 -> Some n0 | _ -> None)
    }

  let lp = char '('
  let rp = char ')'
  let num = compose string integer <$> rep1 digit
  let exp = fix @@ fun exp ->
    operator 
      [ `Infixl (50, addE, (text "+"));
        `Infixl (50, subE, (text "-"));
        `Infixl (40, mulE, (text "*"));
        `Infixl (40, divE, (text "/"));
        `Prefix (10, incE, (text "++"));
        `Prefix (10, decE, (text "--"));
        `Suffix (10, incE, (text "++"));
        `Suffix (10, decE, (text "--"))
      ] ~innermost:((intE <$> num) <|> between lp rp exp)

  let assert_isomorphic syntax (ast, str) =
    let x0 = parse syntax str in
    let x1 = print syntax ast >>= parse syntax in
    let printer ast =
      match ast >>= print syntax with
      | Some z -> z
      | None   -> "*failure*"
    in
    assert_equal ~printer (Some ast) x0;
    assert_equal ~printer (Some ast) x1

  let tt = 
    "Operator_precedence" >:::
      [ "integer-literal" >:: begin fun _ ->
          assert_isomorphic exp (IntE 42, "42");
          assert_isomorphic exp (IntE 42, "(42)");
          assert_isomorphic exp (IntE 42, "((42))");
        end;
        "add-or-sub-expression" >:: begin fun _ ->
          assert_isomorphic exp (AddE (IntE 1, IntE 2), "1+2");
          assert_isomorphic exp (AddE (AddE (IntE 1, IntE 2), IntE 3), "1+2+3");
          assert_isomorphic exp (AddE (AddE (IntE 1, IntE 2), IntE 3), "(1+2)+3");
          assert_isomorphic exp (AddE (IntE 1, AddE (IntE 2, IntE 3)), "1+(2+3)");
          assert_isomorphic exp (SubE (IntE 1, IntE 2), "1-2");
          assert_isomorphic exp (SubE (SubE (IntE 1, IntE 2), IntE 3), "1-2-3");
          assert_isomorphic exp (SubE (SubE (IntE 1, IntE 2), IntE 3), "(1-2)-3");
          assert_isomorphic exp (SubE (IntE 1, SubE (IntE 2, IntE 3)), "1-(2-3)");
          assert_isomorphic exp (SubE (AddE (IntE 1, IntE 2), IntE 3), "1+2-3");
          assert_isomorphic exp (AddE (SubE (IntE 1, IntE 2), IntE 3), "1-2+3");
        end;
        "mul-or-div-expression" >:: begin fun _ ->
          assert_isomorphic exp (MulE (IntE 1, IntE 2), "1*2");
          assert_isomorphic exp (MulE (MulE (IntE 1, IntE 2), IntE 3), "1*2*3");
          assert_isomorphic exp (MulE (MulE (IntE 1, IntE 2), IntE 3), "(1*2)*3");
          assert_isomorphic exp (MulE (IntE 1, MulE (IntE 2, IntE 3)), "1*(2*3)");
          assert_isomorphic exp (DivE (IntE 1, IntE 2), "1/2");
          assert_isomorphic exp (DivE (DivE (IntE 1, IntE 2), IntE 3), "1/2/3");
          assert_isomorphic exp (DivE (DivE (IntE 1, IntE 2), IntE 3), "(1/2)/3");
          assert_isomorphic exp (DivE (IntE 1, DivE (IntE 2, IntE 3)), "1/(2/3)");
          assert_isomorphic exp (DivE (MulE (IntE 1, IntE 2), IntE 3), "1*2/3");
          assert_isomorphic exp (MulE (DivE (IntE 1, IntE 2), IntE 3), "1/2*3");
        end;
        "inc-or-dec-expression" >:: begin fun _ ->
          assert_isomorphic exp ((IncE (IntE 1)), "++1");
          assert_isomorphic exp ((IncE (IntE 1)), "1++");
          assert_isomorphic exp ((DecE (IntE 1)), "--1");
          assert_isomorphic exp ((DecE (IntE 1)), "1--");
          assert_isomorphic exp (IncE (IncE (IntE 1)), "++++1");
          assert_isomorphic exp (IncE (IncE (IntE 1)), "1++++");
          assert_isomorphic exp (DecE (DecE (IntE 1)), "----1");
          assert_isomorphic exp (DecE (DecE (IntE 1)), "1----");
          assert_isomorphic exp (IncE (DecE (IntE 1)), "++--1");
          assert_isomorphic exp (IncE (DecE (IntE 1)), "1--++");
          assert_isomorphic exp (DecE (IncE (IntE 1)), "--++1");
          assert_isomorphic exp (DecE (IncE (IntE 1)), "1++--");
        end;
        "arithmetic-expression" >:: begin fun _ ->
          assert_isomorphic exp (AddE (IntE 1, MulE (IntE 2, IntE 3)), "1+2*3");
          assert_isomorphic exp (AddE (MulE (IntE 1, IntE 2), IntE 3), "1*2+3");
          assert_isomorphic exp (AddE (IncE (IntE 1), MulE (IntE 2, IntE 3)), "++1+2*3");
          assert_isomorphic exp (AddE (MulE (IntE 1, IntE 2), DecE (IntE 3)), "1*2+3--");
        end
      ]
end
