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

type 'a syntax = 'a -> string option
let run = (@@)
let fwd = Iso.fwd
let bwd = Iso.bwd

let (<$>) d0 p0 =
  fun x0 ->
    bwd d0 x0 >>= fun x1 ->
    run p0 x1 >>= fun z0 ->
    return z0

let (<|>) p0 p1 =
  fun x0 ->
    match run p0 x0 with
    | Some z0 ->
      Some z0
    | None ->
      run p1 x0

let (<*>) p0 p1 =
  fun (x0, x1) ->
    run p0 x0 >>= fun z0 ->
    run p1 x1 >>= fun z1 ->
    return (z0 ^ z1)

let fix f =
  let rec fix = lazy (f p)
      and p = 
        fun x0 ->
          run (Lazy.force fix) x0
  in p

let fail =
  fun _ -> None

let pure ?(compare = Pervasives.compare) =
  fun x0 x1 ->
    if (compare x0 x1 = 0) then Some ""
                           else None

let (~!) p0 =
  fun () ->
    match run p0 () with
    | Some _ ->
      Some ""
    | None ->
      None

let (~&) p0 =
  fun () ->
    match run p0 () with
    | Some _ ->
      Some ""
    | None ->
      None

let opt p0 =
  fun () ->
    ignore (run p0 ()); Some ""

let any =
  fun x0 ->
    return (String.make 1 x0)

let apply = run
let print = run
