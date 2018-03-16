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
open Finale.Iso
open Finale.Iso_partial
open Finale.Syntax
open Finale.Parser

module Make (Parser: Parser_intf.S) = struct
  open Parser

  let tt = 
    "Parser" >:::
      [ "any" >:: begin fun _ ->
          assert_equal (Some 'f') @@ parse any "foo";
          assert_equal (Some 'b') @@ parse any "bar";
          assert_equal None @@ parse any ""
        end;
        "(<|>)" >:: begin fun _ ->
          let a = subset ((=) 'a') <$> any in
          let b = subset ((=) 'b') <$> any in
          assert_equal (Some 'a') @@ parse (a <|> b) "a";
          assert_equal (Some 'a') @@ parse (b <|> a) "a";
          assert_equal None @@ parse (a <|> b) "c"
        end;
        "(<*>)" >:: begin fun _ ->
          let a = subset ((=) 'a') <$> any in
          let b = subset ((=) 'b') <$> any in
          assert_equal (Some ('a', 'b')) @@ parse (a <*> b) "ab";
          assert_equal None @@ parse (a <*> b) "ac";
          assert_equal None @@ parse (a <*> b) "cb";
          assert_equal None @@ parse (a <*> b) "a";
          assert_equal None @@ parse (a <*> b) "b"
        end;
        "fail" >:: begin fun _ ->
          assert_equal None @@ parse fail "foo";
          assert_equal None @@ parse fail "bar";
          assert_equal None @@ parse fail ""
        end;
        "pure" >:: begin fun _ ->
          assert_equal (Some ()) @@ parse (pure ()) "foo";
          assert_equal (Some ()) @@ parse (pure ()) "";
          assert_equal (Some ('a', 'b')) @@ parse (pure 'a' <*> any) "bar";
          assert_equal (Some ('b', 'b')) @@ parse (pure 'b' <*> any) "bar"
        end;
        "(~!)" >:: begin fun _ ->
          assert_equal (Some ()) @@ parse (~!fail) "bar";
          assert_equal (Some ((), 'b')) @@ parse (~!fail <*> any) "bar";
          assert_equal None @@ parse (~!(pure ())) "bar";
        end;
        "(~&)" >:: begin fun _ ->
          assert_equal None @@ parse (~&fail) "bar";
          assert_equal (Some ()) @@ parse (~&(pure ())) "bar";
          assert_equal (Some ((), 'b')) @@ parse (~&(pure ()) <*> any) "bar";
        end
      ]
end
