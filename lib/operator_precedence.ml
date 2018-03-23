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
open Iso
open Iso_partial

module Make (Syntax: Syntax_intf.S) = struct
  module Combinator_base = Syntax
  module Combinator = Combinator.Make (Combinator_base)
  open Combinator_base
  open Combinator

  type 'a operator =
    [ `Infixl of int * ('a * 'a, 'a) iso * unit syntax
    | `Infixr of int * ('a * 'a, 'a) iso * unit syntax
    | `Prefix of int * ('a, 'a) iso * unit syntax
    | `Suffix of int * ('a, 'a) iso * unit syntax
    ]
  type 'a operator_spec = 'a operator list

  let chainl operator_spec p0 ~lhs:p1 ~rhs:p2 =
    let iso =
      { fwd = (function (x0, (n0, x1)) -> List.nth operator_spec n0 >>= function `Infixl (_, d0, _) -> d0.fwd (x0, x1));
        bwd = (function x0 -> 
          List.find_mapi operator_spec 
            ~f:(fun n0 -> function `Infixl (_, d0, _) -> d0.bwd x0 >>| fun (x1, x2) -> (x1, (n0, x2))))
      }
    in foldl iso <$> (p1 <*> rep0 (p0 <*> p2))

  let chainr operator_spec p0 ~lhs:p1 ~rhs:p2 =
    let iso =
      { fwd = (function ((x0, n0), x1) -> List.nth operator_spec n0 >>= function `Infixr (_, d0, _) -> d0.fwd (x0, x1));
        bwd = (function x0 ->
          List.find_mapi operator_spec
            ~f:(fun n0 -> function `Infixr (_, d0, _) -> d0.bwd x0 >>| fun (x1, x2) -> ((x1, n0), x2)))
      } 
    in foldr iso <$> (rep0 (p1 <*> p0) <*> p2)


  let rec translate operator_spec_list ~innermost =
    match operator_spec_list with
    | operator_spec :: operator_spec_list ->
      let (infixop, prefixop, suffixop) = List.partition3_map ~f:begin function
          | `Infixl v -> `Fst (`Infixl v)
          | `Infixr v -> `Fst (`Infixr v)
          | `Prefix v -> `Snd (`Prefix v)
          | `Suffix v -> `Trd (`Suffix v)
        end operator_spec in
      let term = translate operator_spec_list ~innermost in
      let prefix = translate_prefixop prefixop ~term in
      let suffix = translate_suffixop suffixop ~term:(translate_infixop infixop ~lhs:prefix ~rhs:term) in
      suffix
    | [] ->
      innermost

  and translate_prefixop operator_spec ~term =
    let operator = choice @@
      List.mapi operator_spec ~f:(fun n0 -> function `Prefix (_, _, p0) -> p0 *> pure (=) n0) in
    let iso =
      { fwd = (function (n0, x0) -> List.nth operator_spec n0 >>= function `Prefix (_, d0, _) -> d0.fwd x0);
        bwd = (function x0 -> 
          List.find_mapi operator_spec
            ~f:(fun n0 -> function `Prefix (_, d0, _) -> d0.bwd x0 >>| fun x1 -> (n0, x1)))
      }
    in foldr iso <$> ((rep0 operator) <*> term)

  and translate_suffixop operator_spec ~term =
    let operator = choice @@
      List.mapi operator_spec ~f:(fun n0 -> function `Suffix (_, _, p0) -> p0 *> pure (=) n0) in
    let iso =
      { fwd = (function (x0, n0) -> List.nth operator_spec n0 >>= function `Suffix (_, d0, _) -> d0.fwd x0);
        bwd = (function x0 ->
          List.find_mapi operator_spec
            ~f:(fun n0 -> function `Suffix (_, d0, _) -> d0.bwd x0 >>| fun x1 -> (x1, n0)))
      }
    in foldl iso <$> (term <*> rep0 operator)

  and translate_infixop operator_spec ~lhs ~rhs =
    let (infixlop, infixrop) =
      List.partition_map operator_spec ~f:begin function
        | `Infixl v -> `Fst (`Infixl v)
        | `Infixr v -> `Snd (`Infixr v)
      end in
    match infixlop, infixrop with
    | infixlop, [] ->
      let operator = choice @@
        List.mapi infixlop ~f:(fun n0 -> function `Infixl (_, _, p0) -> p0 *> pure (=) n0) in
      chainl infixlop operator lhs rhs
    | [], infixrop ->
      let operator = choice @@
        List.mapi infixrop ~f:(fun n0 -> function `Infixr (_, _, p0) -> p0 *> pure (=) n0) in
      chainr infixrop operator lhs rhs
    | _ ->
      failwith "Not supported"

  let operator_prec = function
    | `Infixl (prec, _, _)
    | `Infixr (prec, _, _)
    | `Prefix (prec, _, _)
    | `Suffix (prec, _, _) ->
      prec
    
  let operator operator_spec ~innermost =
    operator_spec
    |> List.sort ~cmp:(fun x0 x1 ->  operator_prec x1 - operator_prec x0)
    |> List.group ~break:(fun x0 x1 -> operator_prec x0 <> operator_prec x1)
    |> translate ~innermost
end
