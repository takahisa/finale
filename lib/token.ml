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
module type S = sig
  include Base.Equal.S
  val of_string: string -> t option
  val to_string: t -> string option
end

let equal (type a) (module Token: S with type t = a) = Token.equal
let of_string (type a) (module Token: S with type t = a) = Token.of_string
let to_string (type a) (module Token: S with type t = a) = Token.to_string

module Char = struct
  type t = char
  let equal = Base.Char.equal
  let of_string x0 = try Some (Base.Char.of_string x0) with Failure _ -> None
  let to_string x0 = try Some (Base.Char.to_string x0) with Failure _ -> None
end
