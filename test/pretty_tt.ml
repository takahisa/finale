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

let pretty_success r e f =
  match Pretty.show (fun () -> r) e with
  | Some x ->
    f x
  | None ->
    assert_failure "printing error"
      
let pretty_failure r e =
  match Pretty.show (fun () -> r) e with
  | Some r ->
    assert_failure "printing error"
  | None ->
    ()

let _ =
  run_test_tt_main begin "Pretty" >::: [
      "char" >:: begin fun _ ->
        pretty_success Pretty.char 'a' (fun real -> assert_equal real "a");
      end;
      "lower" >:: begin fun _ ->
        pretty_success Pretty.lower 'a' (fun real -> assert_equal real "a");
        pretty_failure Pretty.lower 'A'
      end;
      "upper" >:: begin fun _ ->
        pretty_success Pretty.upper 'A' (fun real -> assert_equal real "A");
        pretty_failure Pretty.upper 'a'
      end;
      "digit" >:: begin fun _ ->
        pretty_success Pretty.digit '0' (fun real -> assert_equal real "0");
        pretty_success Pretty.digit '1' (fun real -> assert_equal real "1");
        pretty_failure Pretty.digit 'a'
      end;      
    ]
  end
