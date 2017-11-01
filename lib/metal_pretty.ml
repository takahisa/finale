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
open Metal_source

type 'a repr =
  { run: (position * 'a) -> (position * string) option }

let ( <$> ) d0 p0 =
  { run = fun (at, a0) ->
        match d0.bwd a0 with
        | Some b0 ->
          p0.run (at, b0)
        | None ->
          None
  }
  
let ( <*> ) p0 p1 =
  { run = fun (at, (a0, b0)) ->
        match p0.run (at, a0) with
        | Some (at, s0) -> begin
            match p1.run (at, b0) with
            | Some (at, s1) ->
              Some (at, s0 ^ s1)
            | None ->
              None
          end
        | None ->
          None
  }

let ( <|> ) p0 p1 =
  { run = fun (at, a0) ->
        match p0.run (at, a0) with
        | Some (at, s0) ->
          Some (at, s0)
        | None ->
          p1.run (at, a0)
  }

let ( <* ) p0 p1 =
  { run = fun (at, a0) ->
        match p0.run (at, a0) with
        | Some (at, s0) -> begin
            match p1.run (at, ()) with
            | Some (at, s1) ->
              Some (at, s0 ^ s1)
            | None ->
              None
          end
        | None ->
          None
  }

let ( *> ) p0 p1 =
  { run = fun (at, a0) ->
        match p0.run (at, ()) with
        | Some (at, s0) -> begin
            match p1.run (at, a0) with
            | Some (at, s1) ->
              Some (at, s0 ^ s1)
            | None ->
              None
          end
        | None ->
          None
  }

let hold f =
  let lazy_p = Lazy.from_fun f in
  { run = fun (at, a0) -> (Lazy.force lazy_p).run (at, a0) }

let fail =
  { run = fun _ -> None }

let rec rep p0 acc =
  { run = function
      | (at, a0 :: rest) -> begin
          match p0.run (at, a0) with
          | Some (at, s0) ->
            (rep p0 (s0 :: acc)).run (at, rest)
          | None ->
            None
        end
      | (at, []) ->
        Some (at, String.concat "" @@ List.rev acc)
  }

let rep0 p0 = rep p0 []
let rep1 p0 = rep p0 []

let sep p0 p1 =
  { run = function
      | (at, a0 :: rest) -> begin
          match p1.run (at, a0) with
          | Some (at, s0) ->
            (rep (p0 *> p1) [s0]).run (at, rest)
          | None ->
            None
        end
      | (at, []) ->
        Some (at, "")
  }
let sep0 = sep
let sep1 = sep

let sep_end p0 p1 =
  rep (p1 <* p0) []

let sep_end0 = sep_end
let sep_end1 = sep_end

let spaces =
  { run = fun (at, ()) -> Some (at, " ") }
let spaces0 = spaces
let spaces1 = spaces


let show ?(at = nowhere) f input =
  match (f ()).run (at, input) with
  | Some (_, res) ->
    Some res
  | None ->
    None
