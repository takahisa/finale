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
open Finale
open Finale.Lazy_stream

let tt =
  "Lazy_stream" >:::
    [ "from" >:: begin fun _ ->
         let r = from (fun i -> Some i) in
         assert_equal 0 @@ head r;
         assert_equal 0 @@ head r;
         assert_equal 1 @@ head @@ tail r;
         assert_equal 2 @@ head @@ tail @@ tail @@ r;
      end;
      "head" >:: begin fun _ ->
        assert_equal 'f' @@ head (of_string "foo");
        assert_equal 'b' @@ head (of_string "bar");
        assert_raises Lazy_stream.Empty @@ fun () -> head (of_string "")
      end;
      "tail" >:: begin fun _ ->
        assert_equal 'o' @@ head (tail (of_string "foo"));
        assert_equal 'a' @@ head (tail (of_string "bar"));
        assert_raises Lazy_stream.Empty @@ fun () -> tail (of_string "")
      end;
  ]
