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
type 'a stream = 'a stream_cell Lazy.t
 and 'a stream_cell = Nil | Cons of 'a * 'a stream
type 'a t = 'a stream

exception Empty

let rec from ~f n =
  Lazy.from_fun @@ fun () ->
    match f n with
    | Some x0 ->
      Cons (x0, from ~f (n + 1))
    | None ->
      Nil
let from ~f = from ~f 0

let empty =
  lazy Nil

let head s =
  match Lazy.force s with
  | Nil -> 
    raise Empty
  | Cons (hd, _) ->
    hd

let tail s =
  match Lazy.force s with
  | Nil ->
    raise Empty
  | Cons (_, tl) ->
    tl

let of_stream stream =
  from ~f:(fun _ -> try Some (Stream.next stream) with Stream.Failure -> None)
let of_string string =
  of_stream (Stream.of_string string)
let of_channel channel =
  of_stream (Stream.of_channel channel)
