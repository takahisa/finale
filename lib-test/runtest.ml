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

let tt =
  "Finale" >:::
    [ Lazy_stream_test.tt;
      Iso_test.tt;
      Iso_partial_test.tt;
      (let module Pretty_test = Pretty_test.Make (Pretty) in Pretty_test.tt);
      (let module Parser_test = Parser_test.Make (Parser) in Parser_test.tt);
      (let module Parser_test = Parser_test.Make (Parser.Longest_match) in Parser_test.tt);
      (let module Combinator_test = Combinator_test.Make (Pretty) (Parser) in Combinator_test.tt);
      ("Syntax" >::: begin
        let module F = Syntax.Make (Pretty) (Parser) in
        let module Pretty_test = Pretty_test.Make (F) in
        let module Parser_test = Parser_test.Make (F) in
        [ Pretty_test.tt; Parser_test.tt ]
      end);
    ]

let _ =
  run_test_tt_main tt
