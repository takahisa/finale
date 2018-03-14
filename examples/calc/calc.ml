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
open Core
open Core.Option
open Finale
open Finale.Iso
open Finale.Iso_partial

type 'a exp =
  | AddE: int exp * int exp -> int exp
  | SubE: int exp * int exp -> int exp
  | MulE: int exp * int exp -> int exp
  | DivE: int exp * int exp -> int exp
  | IntE: int -> int exp

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
let intE =
  { fwd = (function n0 -> Some (IntE n0));
    bwd = (function IntE n0 -> Some n0 | _ -> None)
  }

module Calc (Pretty: Syntax.PRETTY) (Parser: Syntax.PARSER) = struct
  module Combinator_base = Syntax.Make (Pretty) (Parser)
  module Combinator = Combinator.Make(Combinator_base)
  open Combinator_base
  open Combinator
  let lp = char '('
  let rp = char ')'

  let infixop_add = char '+'
  let infixop_sub = char '-'
  let infixop_mul = char '*'
  let infixop_div = char '/'

  let num = (compose string integer <$> rep1 digit)
  let exp = fix @@ fun exp ->
        (between lp rp (addE <$> (exp <*> (infixop_add *> exp))))
    <|> (between lp rp (subE <$> (exp <*> (infixop_sub *> exp))))
    <|> (between lp rp (mulE <$> (exp <*> (infixop_mul *> exp))))
    <|> (between lp rp (divE <$> (exp <*> (infixop_div *> exp))))
    <|> (intE <$> num)

  let parse = Combinator_base.parse exp
  let print = Combinator_base.print exp

  let rec eval = function
    | IntE n0 -> n0
    | AddE (e0, e1) -> eval e0 + eval e1
    | SubE (e0, e1) -> eval e0 - eval e1
    | MulE (e0, e1) -> eval e0 * eval e1
    | DivE (e0, e1) -> eval e0 / eval e1
end

let _ =
  let module Calc = Calc (Pretty) (Parser) in
  let result =
    In_channel.input_line In_channel.stdin >>= fun z ->
    Calc.parse z >>= fun e ->
    Calc.print e >>= fun z ->
    return (e, z)
  in
  match result with
  | Some (_, z) ->
    print_endline z
  | None ->
    print_endline "Failure"
