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
open Cycle.Token

module CharTest = struct
  let equal_test =
    "Cycle.Token.Char.test_string" >:: begin fun _ ->
      assert_equal (Char.equal 'a' 'b') false;
      assert_equal (Char.equal 'a' '0') false;
      assert_equal (Char.equal 'a' 'a') true
    end

  let of_string_test =
    "Cycle.Token.Char.of_string" >:: begin fun _ ->
      assert_equal (Char.of_string "") None;
      assert_equal (Char.of_string "foo") None;
      assert_equal (Char.of_string "bar") None;
      assert_equal (Char.of_string "a") (Some 'a');
    end

  let to_string_test =
    "Cycle.Token.Char.to_string" >:: begin fun _ ->
      assert_equal (Char.to_string 'a') (Some "a");
      assert_equal (Char.to_string '0') (Some "0")
    end
end

let equal_test =
  "Cycle.Token.equal" >:: begin fun _ ->
    assert_equal (Token.equal (module Char) 'a' 'b') false;
    assert_equal (Token.equal (module Char) 'a' '0') false;
    assert_equal (Token.equal (module Char) 'a' 'a') true
  end

let of_string_test =
  "Cycle.Token.of_string" >:: begin fun _ ->
    assert_equal (Token.of_string (module Char) "") None;
    assert_equal (Token.of_string (module Char) "foo") None;
    assert_equal (Token.of_string (module Char) "bar") None;
    assert_equal (Token.of_string (module Char) "a") (Some 'a');    
  end

let to_string_test =
  "Cycle.Token.to_string" >:: begin fun _ ->
    assert_equal (Token.to_string (module Char) 'a') (Some "a");
    assert_equal (Token.to_string (module Char) '0') (Some "0")
  end  

let all_test =
  "Cycle.Token" >::: [
    "Cycle.Token.Char" >::: [
      CharTest.equal_test;
      CharTest.of_string_test;
      CharTest.to_string_test
    ];
    equal_test;
    of_string_test;
    to_string_test
  ]
