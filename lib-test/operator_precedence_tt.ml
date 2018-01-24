(*
 * Copyright (c) 2017 Takahisa Watanabe <linerlock@outlook.com> All rights reserved.
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
open Fractale
open Fractale.SYM
open Fractale.Iso

type 'a exp =
  | Int: int -> int exp
  | Add: (int exp * int exp) -> int exp
  | Sub: (int exp * int exp) -> int exp
  | Mul: (int exp * int exp) -> int exp
  | Div: (int exp * int exp) -> int exp

let intE =
  { fwd = (function n -> Some (Int n));
    bwd = (function (Int n) -> Some n | _ -> None)
  }

let addE =
  { fwd = (function (e0, e1) -> Some (Add (e0, e1)));
    bwd = (function (Add (e0, e1)) -> Some (e0, e1) | _ -> None)
  }

let subE =
  { fwd = (function (e0, e1) -> Some (Sub (e0, e1)));
    bwd = (function (Sub (e0, e1)) -> Some (e0, e1) | _ -> None)
  }

let mulE =
  { fwd = (function (e0, e1) -> Some (Mul (e0, e1)));
    bwd = (function (Mul (e0, e1)) -> Some (e0, e1) | _ -> None)
  }

let divE =
  { fwd = (function (e0, e1) -> Some (Div (e0, e1)));
    bwd = (function (Div (e0, e1)) -> Some (e0, e1) | _ -> None)
  }

let implode = fun cs ->
  String.concat "" @@ List.map (fun c -> String.make 1 c) cs

let explode = fun z ->
  let n = String.length z in
  let rec go j =
    if j < n then String.get z j :: go (j+1) else []
  in go 0

let number =
  { fwd = (function cs -> Some (int_of_string @@ implode cs));
    bwd = (function n  -> Some (explode @@ string_of_int n))
  }

module Syntax (M: META_SYNTAX) = struct
  open M
  let lp = char <$ element '('
  let rp = char <$ element ')'

  let rec exp ~prec:n =
    (if n < 1 then (addE <$> hold @@ fun () -> (exp ~prec:1 <*> ((char <$ element '+') *> exp ~prec:0))) else fail) <|>
    (if n < 1 then (subE <$> hold @@ fun () -> (exp ~prec:1 <*> ((char <$ element '-') *> exp ~prec:0))) else fail) <|>
    (if n < 2 then (mulE <$> hold @@ fun () -> (exp ~prec:2 <*> ((char <$ element '*') *> exp ~prec:1))) else fail) <|>
    (if n < 2 then (divE <$> hold @@ fun () -> (exp ~prec:2 <*> ((char <$ element '/') *> exp ~prec:1))) else fail) <|>
    (compose number intE <$> rep1 digit) <|> (between lp rp (hold @@ fun () -> exp ~prec:0))
  let exp = exp 0
end

module ParserTest = struct
  module Syntax = Syntax (Parser)

  let success r s f =
    match Parser.read (fun () -> r) s with
    | Some x ->
      f x
    | None ->
      assert_failure ""

  let failure r s =
    match Parser.read (fun () -> r) s with
    | Some r ->
      assert_failure ""
    | None ->
      ()
  let _ =
    run_test_tt_main begin "OperatorPrecedence(Parser)" >::: [
        "intE" >:: begin fun _ ->
          success Syntax.exp "1" (fun real -> assert_equal real (Int 1));
          success Syntax.exp "10" (fun real -> assert_equal real (Int 10));
        end;
        "addE or subE" >:: begin fun _ ->
          success Syntax.exp "10+20" (fun real -> assert_equal real (Add (Int 10, Int 20)));
          success Syntax.exp "10+20+30" (fun real -> assert_equal real (Add (Int 10, (Add (Int 20, Int 30)))));
          success Syntax.exp "10+20-30" (fun real -> assert_equal real (Add (Int 10, (Sub (Int 20, Int 30)))));
          success Syntax.exp "10-20+30" (fun real -> assert_equal real (Sub (Int 10, (Add (Int 20, Int 30)))));
        end;
        "mulE or divE" >:: begin fun _ ->
          success Syntax.exp "10*20" (fun real -> assert_equal real (Mul (Int 10, Int 20)));
          success Syntax.exp "10*20*30" (fun real -> assert_equal real (Mul (Int 10, (Mul (Int 20, Int 30)))));
          success Syntax.exp "10*20/30" (fun real -> assert_equal real (Mul (Int 10, (Div (Int 20, Int 30)))));
          success Syntax.exp "10/20*30" (fun real -> assert_equal real (Div (Int 10, (Mul (Int 20, Int 30)))));
        end;
        "exp" >:: begin fun _ ->
          success Syntax.exp "10+20*30" (fun real -> assert_equal real (Add (Int 10, (Mul (Int 20, Int 30)))));
          success Syntax.exp "10*20+30" (fun real -> assert_equal real (Add (Mul (Int 10, Int 20), Int 30)));
          success Syntax.exp "(10+20)*30" (fun real -> assert_equal real (Mul (Add (Int 10, Int 20), Int 30)));
          success Syntax.exp "10*(20+30)" (fun real -> assert_equal real (Mul (Int 10,  Add (Int 20, Int 30))));
        end
      ]
    end
end
module PrettyTest = struct
  module Syntax = Syntax (Pretty)

  let success r e f =
    match Pretty.show (fun () -> r) e with
    | Some x ->
      f x
    | None ->
      assert_failure ""

  let failure r e =
    match Pretty.show (fun () -> r) e with
    | Some r ->
      assert_failure ""
    | None ->
      ()
  let _ =
    run_test_tt_main begin "OperatorPrecedence(Pretty)" >::: [
        "intE" >:: begin fun _ ->
          success Syntax.exp (Int 1) (fun real -> assert_equal real "1");
          success Syntax.exp (Int 10) (fun real -> assert_equal real "10")
        end;
        "addE or subE" >:: begin fun _ ->
          success Syntax.exp (Add (Int 10, Int 20)) (fun real -> assert_equal real "10+20");
          success Syntax.exp (Add (Int 10, (Add (Int 20, Int 30)))) (fun real -> assert_equal real "10+20+30");
          success Syntax.exp (Add (Int 10, (Sub (Int 20, Int 30)))) (fun real -> assert_equal real "10+20-30");
          success Syntax.exp (Sub (Int 10, (Add (Int 20, Int 30)))) (fun real -> assert_equal real "10-20+30");
        end;
        "mulE or divE" >:: begin fun _ ->
          success Syntax.exp (Mul (Int 10, Int 20)) (fun real -> assert_equal real "10*20");
          success Syntax.exp (Mul (Int 10, (Mul (Int 20, Int 30)))) (fun real -> assert_equal real "10*20*30");
          success Syntax.exp (Mul (Int 10, (Div (Int 20, Int 30)))) (fun real -> assert_equal real "10*20/30");
          success Syntax.exp (Div (Int 10, (Mul (Int 20, Int 30)))) (fun real -> assert_equal real "10/20*30");
        end;
        "exp" >:: begin fun _ ->
          success Syntax.exp (Add (Int 10, (Mul (Int 20, Int 30)))) (fun real -> assert_equal real "10+20*30");
          success Syntax.exp (Add (Mul (Int 10, Int 20), Int 30)) (fun real -> assert_equal real "10*20+30");
          success Syntax.exp (Mul (Add (Int 10, Int 20), Int 30)) (fun real -> assert_equal real "(10+20)*30");
          success Syntax.exp (Mul (Int 10,  Add (Int 20, Int 30))) (fun real -> assert_equal real "10*(20+30)");
        end
      ]
    end
end
