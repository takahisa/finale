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

let parse_success r s f =
  match Parser.read (fun () -> r) s with
  | Some x ->
    f x
  | None ->
    assert_failure "parse error"
      
let parse_failure r s =
  match Parser.read (fun () -> r) s with
  | Some r ->
    assert_failure "parse error"
  | None ->
    ()

let _ =
  run_test_tt_main begin "Parser" >::: [
      "char" >:: begin fun _ ->
        parse_success Parser.char "a" (fun real -> assert_equal real 'a');
        parse_success Parser.char "ab" (fun real -> assert_equal real 'a');
      end;
      "lower" >:: begin fun _ ->
        parse_success Parser.lower "a" (fun real -> assert_equal real 'a');
        parse_failure Parser.lower "A"
      end;
      "upper" >:: begin fun _ ->
        parse_success Parser.upper "A" (fun real -> assert_equal real 'A');
        parse_failure Parser.upper "a"
      end;
      "digit" >:: begin fun _ ->
        parse_success Parser.digit "0" (fun real -> assert_equal real '0');
        parse_success Parser.digit "1" (fun real -> assert_equal real '1');
        parse_failure Parser.digit "a"
      end;      
    ]
  end
