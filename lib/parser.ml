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
open Core
open Core.Option

type 'a parser = char Lazy_stream.t -> (char Lazy_stream.t * 'a) option
type 'a syntax = 'a parser
let run = (@@)
let fwd = Iso.fwd
let bwd = Iso.bwd

let (<$>) d0 p0 =
  fun r0 ->
    run p0 r0 >>= fun (r1, x1) ->
    fwd d0 x1 >>= fun x1' ->
    return (r1, x1')

let (<|>) p0 p1 =
  fun r0 ->
    match run p0 r0 with
    | Some (r1, x1) ->
      Some (r1, x1)
    | None ->
      run p1 r0

let (<*>) p0 p1 =
  fun r0 ->
    run p0 r0 >>= fun (r1, x1) ->
    run p1 r1 >>= fun (r2, x2) ->
    return (r2, (x1, x2))

let fix f =
  let rec fix = lazy (f p)
      and p =
        fun r0 ->
          run (Lazy.force fix) r0
  in p

let fail =
  fun _ -> None

let pure ?(compare = Pervasives.compare) x0 =
  fun r0 ->
    return (r0, x0)

let skip p0 =
  fun r0 ->
    match run p0 r0 with
    | Some (r1, _) ->
      Some (r1, ())
    | None ->
      None

let (~!) p0 =
  fun r0 ->
    match run p0 r0 with
    | Some (_, ()) ->
      None
    | None ->
      Some (r0, ())

let (~&) p0 =
  fun r0 ->
    match run p0 r0 with
    | Some (_, ()) ->
      Some (r0, ())
    | None ->
      None

let any =
  fun r0 ->
    try
      Some (Lazy_stream.tail r0, Lazy_stream.head r0)
    with Lazy_stream.Empty ->
      None

let apply = run
let parse = 
  fun p0 z0 ->
    run p0 (Lazy_stream.of_string z0) >>| snd
