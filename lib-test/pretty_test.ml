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
open Finale.Pretty

module Make (Pretty: PRETTY) = struct
  let tt = "Pretty" >::: [
      "char" >:: begin fun _ ->
        assert_equal (Some "f") @@ print char 'f'
      end;
      "(<|>)" >:: begin fun _ ->
        let a = subset ((=) 'a') <$> char in
        let b = subset ((=) 'b') <$> char in
        assert_equal (Some "a") @@ print (a <|> b) 'a';
        assert_equal (Some "b") @@ print (a <|> b) 'b';
        assert_equal None @@ print (a <|> b) 'c'
      end;
      "(<*>)" >:: begin fun _ ->
        let a = subset ((=) 'a') <$> char in
        let b = subset ((=) 'b') <$> char in
        assert_equal (Some "ab") @@ print (a <*> b) ('a', 'b');
        assert_equal None @@ print (a <*> b) ('a', 'c');
        assert_equal None @@ print (a <*> b) ('c', 'b');
        assert_equal None @@ print (a <*> b) ('c', 'c')
      end;
      "fail" >:: begin fun _ ->
        assert_equal None @@ print fail ();
        assert_equal None @@ print fail 42
      end;
      "pure" >:: begin fun _ ->
        assert_equal (Some "") @@ print (pure ()) ();
        assert_equal (Some "") @@ print (pure 42) 42;
        assert_equal None @@ print (pure 42) 24
      end
    ]
end
