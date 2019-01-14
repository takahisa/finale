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
type ('a, 'b) t =
  { f: 'a -> 'b option;
    g: 'b -> 'a option
  }

val iso: f:('a -> 'b option) -> g:('b -> 'a option) -> ('a, 'b) t
val f: ('a, 'b) t -> 'a -> 'b option
val g: ('a, 'b) t -> 'b -> 'a option

val identity: ('a, 'a) t
val compose: ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
val inverse: ('a, 'b) t -> ('b, 'a) t
val commute: ('a * 'b, 'b * 'a) t
val product: ('a, 'b) t -> ('c, 'd) t -> ('a * 'c, 'b * 'd) t
val assoc: ('a * ('b * 'c), ('a * 'b) * 'c) t
