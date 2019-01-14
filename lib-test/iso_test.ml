(* 
 * Copyright (c) 2019 Takahisa Watanabe <takahisa.watanabe@outlook.com> All rights reserved.
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
open Cycle
open Cycle.Iso

let identity_test =
  "Cycle.Iso.identity" >:: begin fun _ ->
    assert_equal (Iso.f identity @@ "foo") (Some "foo");
    assert_equal (Iso.g identity @@ "bar") (Some "bar");
  end

let commute_test =
  "Cycle.Iso.commute" >:: begin fun _ ->
    assert_equal (Iso.f commute @@ ("foo", "bar")) (Some ("bar", "foo"));
    assert_equal (Iso.g commute @@ ("foo", "bar")) (Some ("bar", "foo"));
  end

let product_test =
  "Cycle.Iso.product" >:: begin fun _ ->
    assert_equal (Iso.f (product identity identity) @@ ("foo", "bar")) (Some ("foo", "bar"));
    assert_equal (Iso.g (product identity identity) @@ ("foo", "bar")) (Some ("foo", "bar"));
  end

let compose_test =
  "Cycle.Iso.compose" >:: begin fun _ ->
    assert_equal (Iso.f (compose identity identity) @@ "foo") (Some "foo");
    assert_equal (Iso.g (compose identity identity) @@ "bar") (Some "bar");
    assert_equal (Iso.f (compose commute commute) @@ (1, "foo")) (Some (1, "foo"));
    assert_equal (Iso.g (compose commute commute) @@ (1, "foo")) (Some (1, "foo"));
  end

let assoc_test =
  "Cycle.Iso.assoc" >:: begin fun _ ->
    assert_equal (Iso.f assoc @@ ("foo", ("bar", "baz"))) (Some (("foo", "bar"), "baz"));
    assert_equal (Iso.g assoc @@ (("foo", "bar"), "baz")) (Some ("foo", ("bar", "baz")))
  end

let all_test =
  "Cycle.Iso" >::: [
    identity_test;
    commute_test;
    product_test;
    compose_test;
    assoc_test
  ]
