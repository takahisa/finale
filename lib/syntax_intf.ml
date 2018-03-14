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
module type S = sig
  type 'a syntax
  val (<$>): ('a, 'b) Iso.iso -> 'a syntax -> 'b syntax
  val (<|>): 'a syntax -> 'a syntax -> 'a syntax
  val (<*>): 'a syntax -> 'b syntax -> ('a * 'b) syntax
  val fix: ('a syntax -> 'a syntax) -> 'a syntax
  val fail: 'a syntax
  val pure: ?compare:('a -> 'a -> int) -> 'a -> 'a syntax
  val (~!): unit syntax -> unit syntax
  val (~&): unit syntax -> unit syntax
  val opt: unit syntax -> unit syntax
  val any: char syntax
end

module type PARSER = sig
  include S
  type 'a parser
  val parse: 'a syntax -> string -> 'a option
end
module type PRETTY = sig
  include S
  type 'a pretty
  val print: 'a syntax -> 'a -> string option
end
