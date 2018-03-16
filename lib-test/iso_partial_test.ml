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
open OUnit2
open Finale.Iso
open Finale.Iso_partial

type 'a exp =
  | IntE: int -> int exp
  | AddE: int exp * int exp -> int exp
  | SubE: int exp * int exp -> int exp

let intE =
  { fwd = (function n -> Some (IntE n));
    bwd = (function IntE n -> Some n | _ -> None);
  }
let addE =
  { fwd = (function (e0, e1) -> Some (AddE (e0, e1)));
    bwd = (function AddE (e0, e1) -> Some (e0, e1) | _ -> None);
  }
let subE =
  { fwd = (function (e0, e1) -> Some (SubE (e0, e1)));
    bwd = (function SubE (e0, e1) -> Some (e0, e1) | _ -> None);
  }

let rec print = function
  | IntE n0 -> string_of_int n0
  | AddE (e0, e1) -> Printf.sprintf "(%s + %s)" (print e0) (print e1)
  | SubE (e0, e1) -> Printf.sprintf "(%s - %s)" (print e0) (print e1)

let tt = 
  "Iso_partial" >:::
    [ "string" >:: begin fun _ ->
        assert_equal (Some "foo") @@ string.fwd ['f'; 'o'; 'o'];
        assert_equal (Some "bar") @@ string.fwd ['b'; 'a'; 'r'];
        assert_equal (Some "") @@ string.fwd [];
        assert_equal (Some ['f'; 'o'; 'o']) @@ string.bwd "foo";
        assert_equal (Some ['b'; 'a'; 'r']) @@ string.bwd "bar";
        assert_equal (Some []) @@ string.bwd ""
      end;
      "integer" >:: begin fun _ ->
        assert_equal (Some 42) @@ integer.fwd "42";
        assert_equal (Some 0x16) @@ integer.fwd "0x16";
        assert_equal (Some 0b10) @@ integer.fwd "0b10";
        assert_equal None @@ integer.fwd "foo";
        assert_equal (Some "42") @@ integer.bwd 42;
        assert_equal (Some "22") @@ integer.bwd 0x16;
        assert_equal (Some "2")  @@ integer.bwd 0b10
      end;
      "boolean" >:: begin fun _ ->
        assert_equal (Some true)  @@ boolean.fwd "true";
        assert_equal (Some false) @@ boolean.fwd "false";
        assert_equal None @@ boolean.fwd "0";
        assert_equal None @@ boolean.fwd "1";
        assert_equal None @@ boolean.fwd "foo";
        assert_equal (Some "true")  @@ boolean.bwd true;
        assert_equal (Some "false") @@ boolean.bwd false
      end;
      "singleton" >:: begin fun _ ->
        assert_equal (Some (42 :: [])) @@ singleton.fwd 42;
        assert_equal (Some (() :: [])) @@ singleton.fwd ();
        assert_equal None     @@ singleton.bwd (List.init 0 ~f:Fn.id);
        assert_equal (Some 0) @@ singleton.bwd (List.init 1 ~f:Fn.id);
        assert_equal None     @@ singleton.bwd (List.init 2 ~f:Fn.id);
        assert_equal None     @@ singleton.bwd (List.init 3 ~f:Fn.id)
      end;
      "nil" >:: begin fun _ ->
        assert_equal (Some []) @@ nil.fwd ();
        assert_equal (Some ()) @@ nil.bwd (List.init 0 ~f:Fn.id);
        assert_equal None      @@ nil.bwd (List.init 1 ~f:Fn.id);
        assert_equal None      @@ nil.bwd (List.init 2 ~f:Fn.id);
        assert_equal None      @@ nil.bwd (List.init 3 ~f:Fn.id)
      end;
      "cons" >:: begin fun _ ->
        assert_equal (Some ([1; 2; 3])) @@ cons.fwd (1, [2; 3]);
        assert_equal (Some (1, [2; 3])) @@ cons.bwd ([1; 2; 3]);
        assert_equal None               @@ cons.bwd []
      end;
      "snoc" >:: begin fun _ ->
        assert_equal (Some ([1; 2; 3])) @@ snoc.fwd ([1; 2], 3);
        assert_equal (Some ([1; 2], 3)) @@ snoc.bwd ([1; 2; 3]);
        assert_equal None               @@ snoc.bwd []
      end;
      "foldl" >:: begin fun _ ->
        assert_equal (Some (AddE (AddE (IntE 1, IntE 2), IntE 3)))
          @@ (foldl addE).fwd (IntE 1, [IntE 2; IntE 3]);
        assert_equal (Some (IntE 1, [IntE 2; IntE 3]))
          @@ (foldl addE).bwd (AddE (AddE (IntE 1, IntE 2), IntE 3));
        assert_equal (Some (IntE 1))     @@ (foldl addE).fwd (IntE 1, []);
        assert_equal (Some (IntE 1, [])) @@ (foldl addE).bwd (IntE 1)
      end;
      "foldr" >:: begin fun _ ->
        assert_equal (Some (AddE (IntE 1, AddE (IntE 2, IntE 3))))
          @@ (foldr addE).fwd ([IntE 1; IntE 2], IntE 3);
        assert_equal (Some ([IntE 1; IntE 2], IntE 3))
          @@ (foldr addE).bwd (AddE (IntE 1, AddE (IntE 2, IntE 3)));
        assert_equal (Some (IntE 1))     @@ (foldr addE).fwd ([], IntE 1);
        assert_equal (Some ([], IntE 1)) @@ (foldr addE).bwd (IntE 1)
      end;
      "fst" >:: begin fun _ ->
        assert_equal (Some 'a')       @@ (fst (element ())).fwd ('a', ());
        assert_equal (Some ('a', ())) @@ (fst (element ())).bwd 'a';
      end;
      "snd" >:: begin fun _ ->
        assert_equal (Some 'a')       @@ (snd (element ())).fwd ((), 'a');
        assert_equal (Some ((), 'a')) @@ (snd (element ())).bwd 'a';
      end;
      "some" >:: begin fun _ ->
        assert_equal (Some (Some 'a')) @@ some.fwd 'a';
        assert_equal (Some 'a')        @@ some.bwd (Some 'a')
      end;
      "none" >:: begin fun _ ->
        assert_equal (Some None) @@ none.fwd ();
        assert_equal (Some ())   @@ none.bwd None
      end
    ]
