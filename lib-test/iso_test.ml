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

let tt = "Iso" >::: [
    "string" >:: begin fun _ ->
      assert_equal (Some "foo") @@ fwd string ['f'; 'o'; 'o'];
      assert_equal (Some "") @@ fwd string [];
      assert_equal (Some ['f'; 'o'; 'o']) @@ bwd string "foo";
      assert_equal (Some [])              @@ bwd string "";
    end;
    "integer" >:: begin fun _ ->
      assert_equal (Some 42) @@ fwd integer "42";
      assert_equal (Some "42") @@ bwd integer 42;
      assert_equal (Some (-42)) @@ fwd integer "-42";
      assert_equal (Some "-42") @@ bwd integer (-42);
      assert_equal None @@ fwd integer "";
      assert_equal None @@ fwd integer "invalid";
    end;
    "boolean" >:: begin fun _ ->
      assert_equal (Some true) @@ fwd boolean "true";
      assert_equal (Some "true") @@ bwd boolean true;
      assert_equal (Some false) @@ fwd boolean "false";
      assert_equal (Some "false") @@ bwd boolean false;
      assert_equal None @@ fwd boolean "";
      assert_equal None @@ fwd boolean "invalid";
    end;
    "identity" >:: begin fun _ ->
      assert_equal (Some 42) @@ fwd identity 42;
      assert_equal (Some 42) @@ bwd identity 42
    end;
    "commute" >:: begin fun _ ->
      assert_equal (Some (1, 2)) @@ fwd commute (2, 1);
      assert_equal (Some (1, 2)) @@ bwd commute (2, 1);
    end;
    "compose" >:: begin fun _ ->
      assert_equal (Some (2, 1)) @@ fwd (compose commute commute) (2, 1)
    end;
    "inverse" >:: begin fun _ ->
      assert_equal (Some 42) @@ fwd integer "42";
      assert_equal (Some 42) @@ bwd (inverse integer) "42";
      assert_equal (Some "42") @@ fwd (inverse integer) 42;
      assert_equal (Some "42") @@ bwd integer 42;
    end;
    "product" >:: begin fun _ ->
      assert_equal (Some (42, true)) @@ fwd (product integer boolean) ("42", "true");
      assert_equal None @@ fwd (product integer boolean) ("invalid", "true");
      assert_equal None @@ fwd (product integer boolean) ("42", "invalid");
    end;
    "associate" >:: begin fun _ ->
      assert_equal (Some ((1, 2), 3)) @@ fwd associate (1, (2, 3));
      assert_equal (Some (1, (2, 3))) @@ bwd associate ((1, 2), 3);
    end;
    "subset" >:: begin fun _ ->
      assert_equal (Some 'a') @@ fwd (subset ((=) 'a')) 'a';
      assert_equal (Some 'a') @@ bwd (subset ((=) 'a')) 'a';
      assert_equal None @@ fwd (subset ((=) 'a')) 'b';
      assert_equal None @@ bwd (subset ((=) 'a')) 'b';
    end
]
