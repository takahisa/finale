(*
 * Copyright (c) 2017 Takahisa Watanabe <linerlock@outlook.com> All rights reserved.
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
open Metal_iso

module type META_SYNTAX = sig
  type 'a repr
  val ( <$> ): ('a, 'b) iso -> 'a repr -> 'b repr
  val ( <*> ): 'a repr -> 'b repr -> ('a * 'b) repr
  val ( <|> ): 'a repr -> 'a repr -> 'a repr

  val ( <* ): 'a repr -> unit repr -> 'a repr
  val ( *> ): unit repr -> 'a repr -> 'a repr

  val ( <$ ): 'a repr -> (unit, 'a) iso -> unit repr
  val ( $> ): 'a repr -> (unit, 'a) iso -> 'a repr

  val hold: (unit -> 'a repr) -> 'a repr
  val fail: 'a repr
  val succeed: unit repr

  val count: int -> 'a repr -> 'a list repr
  val rep0: 'a repr -> 'a list repr
  val rep1: 'a repr -> 'a list repr

  val sep0: unit repr -> 'a repr -> 'a list repr
  val sep1: unit repr -> 'a repr -> 'a list repr

  val sep_end0: unit repr -> 'a repr -> 'a list repr
  val sep_end1: unit repr -> 'a repr -> 'a list repr

  val spaces0: unit repr
  val spaces1: unit repr
  val between: unit repr -> unit repr -> 'a repr -> 'a repr

  val chainl1: ('a * 'a, 'a) iso -> unit repr -> 'a repr -> 'a repr
  val chainr1: ('a * 'a, 'a) iso -> unit repr -> 'a repr -> 'a repr

  val char: char repr
  val lower: char repr
  val upper: char repr
  val digit: char repr
end
