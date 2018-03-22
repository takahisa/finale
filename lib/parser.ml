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

type 'a token = { lexeme: 'a; position: int }
type 'a parser = char token Lazy_stream.t -> (char token Lazy_stream.t * 'a) option
type 'a syntax = 'a parser
let run = (@@)
let fwd = Iso.fwd
let bwd = Iso.bwd

module Token_stream = struct
  include Lazy_stream
      
  let of_stream stream =
    let source_stream = ref stream in
    Lazy_stream.from begin fun i ->
      try
        let lexeme = Lazy_stream.head !source_stream in
        source_stream := Lazy_stream.tail !source_stream;
        Some { lexeme; position = i }
      with Lazy_stream.Empty ->
        None
    end
      
  let of_string string =
    of_stream @@ Lazy_stream.of_string string
      
  let of_channel channel =
    of_stream @@ Lazy_stream.of_channel channel
end


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

let pure (=) x0 =
  fun r0 ->
    return (r0, x0)

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

let opt p0 =
  fun r0 ->
    match run p0 r0 with
    | Some (r1, _) ->
      Some (r1, ())
    | None ->
      Some (r0, ())

let any =
  fun r0 ->
    try
      Some (Token_stream.tail r0, (Token_stream.head r0).lexeme)
    with Token_stream.Empty ->
      None

let apply = run
let parse = 
  fun p0 z0 ->
    run p0 (Token_stream.of_string z0) >>| snd

module Longest_match = struct
  type nonrec 'a parser = 'a parser
  type nonrec 'a syntax = 'a syntax
  let (<$>) = (<$>)
  let (<*>) = (<*>)
  let fix = fix
  let fail = fail
  let pure = pure
  let (~!) = (~!)
  let (~&) = (~&)
  let opt = opt
  let any = any
  let apply = apply
  let parse = parse

  let (<|>) p0 p1 =
    fun r0 ->
      match run p0 r0, run p1 r0 with
      | Some (r10, x10), Some (r11, x11) ->
        let n0 = try (Token_stream.head r10).position with Lazy_stream.Empty -> Int.max_value in
        let n1 = try (Token_stream.head r11).position with Lazy_stream.Empty -> Int.max_value in
        if (n0 >= n1) then Some (r10, x10) else Some (r11, x11)
      | Some (r10, x10), _ ->
        Some (r10, x10)
      | _, Some (r11, x11) ->
        Some (r11, x11)
      | _ ->
        None
end
