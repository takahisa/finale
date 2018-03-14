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
type ('a, 'b) iso =
  { fwd: 'a -> 'b option;
    bwd: 'b -> 'a option
  }
val iso: fwd:('a -> 'b option) -> bwd:('b -> 'a option) -> ('a, 'b) iso
val fwd: ('a, 'b) iso -> 'a -> 'b option
val bwd: ('a, 'b) iso -> 'b -> 'a option

val identity: ('a, 'a) iso
val compose: ('a, 'b) iso -> ('b, 'c) iso -> ('a, 'c) iso
val inverse: ('a, 'b) iso -> ('b, 'a) iso
val commute: ('a * 'b, 'b * 'a) iso
val product: ('a, 'b) iso -> ('c, 'd) iso -> ('a * 'c, 'b * 'd) iso
val associate: ('a * ('b * 'c), ('a * 'b) * 'c) iso
