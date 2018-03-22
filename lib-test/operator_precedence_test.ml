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
    | NegE: int exp -> int exp
    | FactorialE: int exp -> int exp
        
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

  let negE =
    { fwd = (function e0 -> Some (NegE e0));
      bwd = (function NegE e0 -> Some e0 | _ -> None)
    }

  let intE =
    { fwd = (function n0 -> Some (IntE n0));
      bwd = (function IntE n0 -> Some n0 | _ -> None)
    }

  let factorialE =
    { fwd = (function e0 -> Some (FactorialE e0));
      bwd = (function FactorialE e0 -> Some e0| _ -> None)
    }

  let operator_spec =
    [ (infixlop ~prec:20 addE (char '+'));
      (infixlop ~prec:20 subE (char '-'));
      (infixlop ~prec:40 mulE (char '*'));
      (infixlop ~prec:40 divE (char '/'));
      (prefixop ~prec:90 negE (char '-'));
      (suffixop ~prec:90 factorialE (char '!'));
    ]

  let num = (compose string integer <$> rep1 digit)
  let exp = fix @@ fun exp ->
    operator operator_spec ~innermost:(intE <$> num <|> between (char '(') (char ')') exp)

  let rec eval = function
    | IntE n0 -> n0
    | NegE e0 -> -(eval e0)
    | AddE (e0, e1) -> eval e0 + eval e1
    | SubE (e0, e1) -> eval e0 - eval e1
    | MulE (e0, e1) -> eval e0 * eval e1
    | DivE (e0, e1) -> eval e0 / eval e1
    | FactorialE e0 ->
      let rec factorial x =
        if x <= 1 then 1 else factorial (x - 1) * x
      in factorial (eval e0)

  let tt = 
    let case z0 n0 =
      z0 >:: fun _ ->
        begin
          parse exp z0 >>= fun e0 ->
          print exp e0 >>= fun z1 -> parse exp z1 >>= fun e1 ->
          assert_equal e0 e1;
          assert_equal n0 (eval e0);
          return ()
        end |> ignore
    in
    "Operator_precedence" >::: 
      [ case "42"      (42);
        case "(42)"    (42);
        case "((42))"  (42);
        case "-42"     (-42);
        case "42!"     (eval (FactorialE (IntE 42)));
        case "1+2"     (1+2);
        case "1-2"     (1-2);
        case "1*2"     (1*2);
        case "1/2"     (1/2);
        case "1+2*3"   (1+2*3);
        case "(1+2)*3" ((1+2)*3);
        case "-1+2*3"  ((-1)+2*3);
        case "1*2+3!"  (1*2+(3*2*1));
      ]
end
