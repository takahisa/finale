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
open Core
open Core.Option
open Finale
open Finale.Iso
open Finale.Iso_partial

module Syntax = Syntax.Make(Pretty)(Parser)
module Syntax_combinators = Combinator.Make(Syntax)
open Syntax
open Syntax_combinators

let tt = "Combinator" >::: [
    "(*>)" >:: begin fun _ ->
      let a = element 'a' <$> char in
      let b = element 'b' <$> char in
      assert_equal (Some "ab") @@ (print (a *> b) ());
      assert_equal (Some ()) @@ (parse (a *> b) "ab");
    end;
    "(<*)" >:: begin fun _ ->
      let a = element 'a' <$> char in
      let b = element 'b' <$> char in
      assert_equal (Some "ab") @@ (print (a <* b) ());
      assert_equal (Some ()) @@ (parse (a <* b) "ab");
    end;
    "sequence" >:: begin fun _ ->
      assert_equal (Some "bar") @@ print (sequence [char; char; char]) ['b'; 'a'; 'r'];
      assert_equal (Some ['b'; 'a'; 'r']) @@ parse (sequence [char; char; char]) "bar"
    end;
    "choice" >:: begin fun _ ->
      let a = subset ((=) 'a') <$> char in
      let b = subset ((=) 'b') <$> char in
      let c = subset ((=) 'c') <$> char in
      assert_equal (Some "a") @@ print (choice [a; b; c]) 'a';
      assert_equal (Some "b") @@ print (choice [a; b; c]) 'b';
      assert_equal (Some "c") @@ print (choice [a; b; c]) 'c';
      assert_equal None @@ print (choice [a; b; c]) 'd';
      assert_equal (Some 'a') @@ parse (choice [a; b; c]) "a";
      assert_equal (Some 'b') @@ parse (choice [a; b; c]) "b";
      assert_equal (Some 'c') @@ parse (choice [a; b; c]) "c";
      assert_equal None @@ parse (choice [a; b; c]) "d";
    end;
    "option" >:: begin fun _ ->
      assert_equal None @@ print (option fail) (Some ());
      assert_equal None @@ print (option fail) (Some 42);
      assert_equal (Some None) @@ parse (option fail) "bar";
      assert_equal (Some None) @@ parse (option fail) "baz";
      assert_equal (Some "a") @@ print (option char) (Some 'a');
      assert_equal (Some (Some 'a')) @@ parse (option char) "a"
    end;
    "count" >:: begin fun _ ->
      assert_equal (Some "bar") @@ print (count 3 char) ['b'; 'a'; 'r'];
      assert_equal None @@ print (count 2 char) ['b'; 'a'; 'r'];
      assert_equal None @@ print (count 4 char) ['b'; 'a'; 'r'];
      assert_equal (Some ['b'; 'a'; 'r']) @@ parse (count 3 char) "bar";
      assert_equal (Some ['b'; 'a'; 'r']) @@ parse (count 3 char) "barz";
      assert_equal None @@ parse (count 3 char) "ba";
    end;
    "rep0" >:: begin fun _ ->
      assert_equal (Some "bar") @@ print (rep0 char) ['b'; 'a'; 'r'];
      assert_equal (Some ['b'; 'a'; 'r']) @@ parse (rep0 char) "bar";
      assert_equal (Some "") @@ print (rep0 char) [];
      assert_equal (Some []) @@ parse (rep0 char) "";
    end;
    "rep1" >:: begin fun _ ->
      assert_equal (Some "bar") @@ print (rep1 char) ['b'; 'a'; 'r'];
      assert_equal (Some ['b'; 'a'; 'r']) @@ parse (rep1 char) "bar";
      assert_equal None @@ print (rep1 char) [];
      assert_equal None @@ parse (rep1 char) "";
    end;
    "sep_by0" >:: begin fun _ ->
      let comma = element ',' <$> char in
      assert_equal (Some "b,a,r") @@ print (sep_by0 ~delimiter:comma char) ['b'; 'a'; 'r'];
      assert_equal (Some ['b'; 'a'; 'r']) @@ parse (sep_by0 ~delimiter:comma char) "b,a,r";
      assert_equal (Some "") @@ print (sep_by0 ~delimiter:comma char) [];
      assert_equal (Some []) @@ parse (sep_by0 ~delimiter:comma char) "";
      assert_equal (Some ['b']) @@ parse (sep_by0 ~delimiter:comma char) "b";
      assert_equal (Some ['b']) @@ parse (sep_by0 ~delimiter:comma char) "b,";
    end;
    "sep_by1" >:: begin fun _ ->
      let comma = element ',' <$> char in
      assert_equal (Some "b,a,r") @@ print (sep_by1 ~delimiter:comma char) ['b'; 'a'; 'r'];
      assert_equal (Some ['b'; 'a'; 'r']) @@ parse (sep_by1 ~delimiter:comma char) "b,a,r";
      assert_equal None @@ print (sep_by1 ~delimiter:comma char) [];
      assert_equal None @@ parse (sep_by1 ~delimiter:comma char) "";
      assert_equal (Some ['b']) @@ parse (sep_by1 ~delimiter:comma char) "b";
      assert_equal (Some ['b']) @@ parse (sep_by1 ~delimiter:comma char) "b,";
    end;
    "end_by0" >:: begin fun _ ->
      let comma = element ',' <$> char in
      assert_equal (Some "b,a,r,") @@ print (end_by0 ~delimiter:comma char) ['b'; 'a'; 'r'];
      assert_equal (Some ['b'; 'a'; 'r']) @@ parse (end_by0 ~delimiter:comma char) "b,a,r,";
      assert_equal (Some "") @@ print (end_by0 ~delimiter:comma char) [];
      assert_equal (Some []) @@ parse (end_by0 ~delimiter:comma char) "";
      assert_equal (Some []) @@ parse (end_by0 ~delimiter:comma char) "b";
      assert_equal (Some ['b']) @@ parse (end_by0 ~delimiter:comma char) "b,";
    end;
    "end_by1" >:: begin fun _ ->
      let comma = element ',' <$> char in
      assert_equal (Some "b,a,r,") @@ print (end_by1 ~delimiter:comma char) ['b'; 'a'; 'r'];
      assert_equal (Some ['b'; 'a'; 'r']) @@ parse (end_by1 ~delimiter:comma char) "b,a,r,";
      assert_equal None @@ print (end_by1 ~delimiter:comma char) [];
      assert_equal None @@ parse (end_by1 ~delimiter:comma char) "";
      assert_equal None @@ parse (end_by1 ~delimiter:comma char) "b";
      assert_equal (Some ['b']) @@ parse (end_by1 ~delimiter:comma char) "b,";
    end;
    "sep_end_by0" >:: begin fun _ ->
      let comma = element ',' <$> char in
      assert_equal (Some "b,a,r") @@ print (sep_end_by0 ~delimiter:comma char) ['b'; 'a'; 'r'];
      assert_equal (Some ['b'; 'a'; 'r']) @@ parse (sep_end_by0 ~delimiter:comma char) "b,a,r";
      assert_equal (Some ['b'; 'a'; 'r']) @@ parse (sep_end_by0 ~delimiter:comma char) "b,a,r,";
      assert_equal (Some "") @@ print (sep_end_by0 ~delimiter:comma char) [];
      assert_equal (Some []) @@ parse (sep_end_by0 ~delimiter:comma char) "";
      assert_equal (Some ['b']) @@ parse (sep_end_by0 ~delimiter:comma char) "b";
      assert_equal (Some ['b']) @@ parse (sep_end_by0 ~delimiter:comma char) "b,";
    end;
    "sep_end_by1" >:: begin fun _ ->
      let comma = element ',' <$> char in
      assert_equal (Some "b,a,r") @@ print (sep_end_by1 ~delimiter:comma char) ['b'; 'a'; 'r'];
      assert_equal (Some ['b'; 'a'; 'r']) @@ parse (sep_end_by1 ~delimiter:comma char) "b,a,r,";
      assert_equal (Some ['b'; 'a'; 'r']) @@ parse (sep_end_by1 ~delimiter:comma char) "b,a,r,";
      assert_equal None @@ print (sep_end_by1 ~delimiter:comma char) [];
      assert_equal None @@ parse (sep_end_by1 ~delimiter:comma char) "";
      assert_equal (Some ['b']) @@ parse (sep_end_by1 ~delimiter:comma char) "b";
      assert_equal (Some ['b']) @@ parse (sep_end_by1 ~delimiter:comma char) "b,";
    end;
    "lower" >:: begin fun _ ->
      assert_equal (Some "a") @@ print lower 'a';
      assert_equal (Some "z") @@ print lower 'z';
      assert_equal (Some 'a') @@ parse lower "a";
      assert_equal (Some 'z') @@ parse lower "z";
      assert_equal None @@ print lower 'A';
      assert_equal None @@ print lower 'Z';
      assert_equal None @@ parse lower "A";
      assert_equal None @@ parse lower "Z";
      assert_equal None @@ print lower '0';
      assert_equal None @@ print lower '9';
      assert_equal None @@ parse lower "0";
      assert_equal None @@ parse lower "9";
    end;
    "upper" >:: begin fun _ ->
      assert_equal (Some "A") @@ print upper 'A';
      assert_equal (Some "Z") @@ print upper 'Z';
      assert_equal (Some 'A') @@ parse upper "A";
      assert_equal (Some 'Z') @@ parse upper "Z";
      assert_equal None @@ print upper 'a';
      assert_equal None @@ print upper 'z';
      assert_equal None @@ parse upper "a";
      assert_equal None @@ parse upper "z";
      assert_equal None @@ print upper '0';
      assert_equal None @@ print upper '9';
      assert_equal None @@ parse upper "0";
      assert_equal None @@ parse upper "9";
    end;
    "alpha" >:: begin fun _ ->
      assert_equal (Some "a") @@ print lower 'a';
      assert_equal (Some "z") @@ print lower 'z';
      assert_equal (Some 'a') @@ parse lower "a";
      assert_equal (Some 'z') @@ parse lower "z";
      assert_equal (Some "A") @@ print upper 'A';
      assert_equal (Some "Z") @@ print upper 'Z';
      assert_equal (Some 'A') @@ parse upper "A";
      assert_equal (Some 'Z') @@ parse upper "Z";
      assert_equal None @@ print upper '0';
      assert_equal None @@ print upper '9';
      assert_equal None @@ parse upper "0";
      assert_equal None @@ parse upper "9";
    end;
    "digit" >:: begin fun _ ->
      assert_equal (Some "0") @@ print digit '0';
      assert_equal (Some "9") @@ print digit '9';
      assert_equal (Some '0') @@ parse digit "0";
      assert_equal (Some '9') @@ parse digit "9";
      assert_equal None @@ print digit 'a';
      assert_equal None @@ print digit 'z';
      assert_equal None @@ parse digit "a";
      assert_equal None @@ parse digit "z";
      assert_equal None @@ print digit 'A';
      assert_equal None @@ print digit 'Z';
      assert_equal None @@ parse digit "A";
      assert_equal None @@ parse digit "Z";
    end;
    "space" >:: begin fun _ ->
      assert_equal (Some "") @@ print space ();
      assert_equal (Some ()) @@ parse space " ";
      assert_equal (Some ()) @@ parse space "\t";
      assert_equal (Some ()) @@ parse space "\r";
      assert_equal (Some ()) @@ parse space "\n";
      assert_equal None @@ parse space "a";
      assert_equal None @@ parse space "0";
    end;
    "space0" >:: begin fun _ ->
      assert_equal (Some "") @@ print spaces0 ();
      assert_equal (Some ()) @@ parse spaces0 " \t\r\n";
      assert_equal (Some ()) @@ parse spaces0 "";
      assert_equal (Some 'a') @@ parse (spaces0 *> char) " \t\r\na";
      assert_equal (Some '0') @@ parse (spaces0 *> char) " \t\r\n0";
    end;
    "space1" >:: begin fun _ ->
      assert_equal (Some " ") @@ print spaces1 ();
      assert_equal (Some ()) @@ parse spaces1 " \t\r\n";
      assert_equal None @@ parse spaces1 "";
      assert_equal (Some 'a') @@ parse (spaces1 *> char) " \t\r\na";
      assert_equal (Some '0') @@ parse (spaces1 *> char) " \t\r\n0";
    end
  ]
