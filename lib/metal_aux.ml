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
module Option = struct
  type 'a t = 'a option

  let map: f:('a -> 'b) -> 'a t -> 'b t = fun ~f -> function
    | Some x0 ->
      Some (f x0)
    | None ->
      None

  let concat_map: f:('a -> 'b t) -> 'a t -> 'b t = fun ~f -> function
    | Some x0 ->
      f x0
    | None ->
      None

  let get: 'a t -> 'a = function
    | Some x0 ->
      x0
    | None ->
      raise (Invalid_argument "None")

  let ( /// ): 'a t -> 'a t -> 'a t =
    fun z0 -> fun z1 ->
      match z0 with
      | Some x0 ->
        Some x0
      | None ->
        z1

  let ( *** ): 'a t -> 'a t -> ('a * 'a) t =
    fun z0 -> fun z1 ->
      match z0, z1 with
      | Some x0, Some x1 ->
        Some (x0, x1)
      | _ ->
        None
end
