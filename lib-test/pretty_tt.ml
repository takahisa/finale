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
open Metal
open Metal.Iso

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

type 'a exp = Base of 'a | Binop of 'a exp * 'a exp
let base_iso =
  { fwd = (function x0 -> Some (Base x0));
    bwd = (function Base x0 -> Some x0 | _ -> None)
  }
let binop_iso =
  { fwd = (function (e0, e1) -> Some (Binop (e0, e1)));
    bwd = (function Binop (e0, e1) -> Some (e0, e1) | _ -> None)
  }


let _ =
  run_test_tt_main begin "Pretty" >::: [
      "text" >:: begin fun _ ->
        success (Pretty.text "abc") "abc" (fun real -> assert_equal real "abc");
        success (Pretty.text "abc") "abcdef" (fun real -> assert_equal real "abc");
        failure (Pretty.text "abc") "";
        failure (Pretty.text "abc") "def";
        success Pretty.(text "abc" <*> text "def") ("abc", "def") (fun real -> assert_equal real "abcdef");
      end;
      "char" >:: begin fun _ ->
        success Pretty.char 'a' (fun real -> assert_equal real "a");
      end;
      "lower" >:: begin fun _ ->
        success Pretty.lower 'a' (fun real -> assert_equal real "a");
        failure Pretty.lower 'A'
      end;
      "upper" >:: begin fun _ ->
        success Pretty.upper 'A' (fun real -> assert_equal real "A");
        failure Pretty.upper 'a'
      end;
      "digit" >:: begin fun _ ->
        success Pretty.digit '0' (fun real -> assert_equal real "0");
        success Pretty.digit '1' (fun real -> assert_equal real "1");
        failure Pretty.digit 'a'
      end;
      "chainl1" >:: begin fun _ ->
        success Pretty.(chainl1 binop_iso (char <$ element '+') (base_iso <$> char)) (Binop (Binop (Base '1', Base '2'), Base '3'))
          (fun real -> assert_equal real "1+2+3");
        failure Pretty.(chainl1 binop_iso (char <$ element '+') (base_iso <$> char)) (Binop (Base '1', Binop (Base '2', Base '3')))
      end;
      "chainr1" >:: begin fun _ ->
        success Pretty.(chainr1 binop_iso (char <$ element '+') (base_iso <$> char)) (Binop (Base '1', Binop (Base '2', Base '3')))
          (fun real -> assert_equal real "1+2+3");
        failure Pretty.(chainr1 binop_iso (char <$ element '+') (base_iso <$> char)) (Binop (Binop (Base '1', Base '2'), Base '3'))
      end;
      "count" >:: begin fun _ ->
        success Pretty.(count 0 char) []
          (fun real -> assert_equal real "");
        success Pretty.(count 3 char) ['a'; 'b'; 'c']
          (fun real -> assert_equal real "abc");
        failure Pretty.(count 2 char) ['a'; 'b'; 'c'];
        failure Pretty.(count 4 char) ['a'; 'b'; 'c'];
        assert_raises (Invalid_argument "count") (fun () -> Pretty.(show (fun () -> count (-1) char) ['a'; 'b'; 'c']));
      end
    ]
  end
