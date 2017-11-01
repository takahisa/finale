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
open Re
open Re_pcre

type 'a repr =
  { run: (position * string) -> (position * string * 'a) option }

let ( <$> ) d0 p0 =
  { run = fun (at, s0) ->
        match p0.run (at, s0) with
        | Some (at, s1, a0) -> begin
            match d0.fwd a0 with
            | Some b0 ->
              Some (at, s1, b0)
            | None ->
              None
          end
        | None ->
          None
  }

let ( <*> ) p0 p1 =
  { run = fun (at, s0) ->
        match p0.run (at, s0) with
        | Some (at, s1, a0) -> begin
            match p1.run (at, s1) with
            | Some (at, s2, a1) ->
              Some (at, s2, (a0, a1))
            | None ->
              None
          end
        | None ->
          None
  }

let ( <|> ) p0 p1 =
  { run = fun (at, s0) ->
        match p0.run (at, s0) with
        | Some (at, s1, a0) ->
          Some (at, s1, a0)
        | None ->
          p1.run (at, s0)
  }

let ( <* ) p0 p1 =
  { run = fun (at, s0) ->
        match p0.run (at, s0) with
        | Some (at, s1, a0) -> begin
            match p1.run (at, s1) with
            | Some (at, s2, ()) ->
              Some (at, s2, a0)
            | None ->
              None
          end
        | None ->
          None
  }

let ( *> ) p0 p1 =
  { run = fun (at, s0) ->
        match p0.run (at, s0) with
        | Some (at, s1, ()) -> begin
            match p1.run (at, s1) with
            | Some (at, s2, a0) ->
              Some (at, s2, a0)
            | None ->
              None
          end
        | None ->
          None
  }

let hold f =
  let lazy_p = Lazy.from_fun f in
  { run = fun (at, s0) -> (Lazy.force lazy_p).run (at, s0) }

let fail =
  { run = fun _ -> None }

let rec rep0 p0 acc =
  { run = fun (at, s0) ->
        match p0.run (at, s0) with
        | Some (at, s1, a0) ->
          (rep0 p0 (a0 :: acc)).run (at, s1)
        | None ->
          Some (at, s0, List.rev acc)
  }
let rep0 p0 = rep0 p0 []
let rep1 p0 =
  { run = fun (at, s0) ->
        match (p0 <*> rep0 p0).run (at, s0) with
        | Some (at, s1, (a0, as0)) ->
          Some (at, s1, a0 :: as0)
        | None ->
          None
  }

let sep1 p0 p1 =
  { run = fun (at, s0) ->
        match (p1 <*> (rep0 (p0 *> p1))).run (at, s0) with
        | Some (at, s1, (a0, as0)) ->
          Some (at, s1, a0 :: as0)
        | None ->
          None
  }

let sep0 p0 p1 =
  { run = fun (at, s0) ->
        match (p1 <*> (rep0 (p0 *> p1))).run (at, s0) with
        | Some (at, s1, (a0, as0)) ->
          Some (at, s1, a0 :: as0)
        | None ->
          Some (at, s0, [])
  }

let sep_end0 p0 p1 =
  rep0 (p1 <* p0)

let sep_end1 p0 p1 =
  rep1 (p1 <* p0)

let spaces0_re = Re_pcre.regexp "^([\\s\\r\\n]*)(.*)"
let spaces0 =
  { run = fun (at, s0) ->
        match Re.exec_opt spaces0_re s0 with
        | Some g when Group.test g 1 && Group.test g 2 ->
          let s0 = Group.get g 1 in
          let s1 = Group.get g 2 in
          Some ({ at with column = at.column + String.length s1 }, s1, ignore s0)
        | _ ->
          None
  }
  
let spaces1_re = Re_pcre.regexp "^([\\s\\r\\n]+)(.*)"
let spaces1 =
  { run = fun (at, s0) ->
        match Re.exec_opt spaces1_re s0 with
        | Some g when Group.test g 1 && Group.test g 2 ->
          let s0 = Group.get g 1 in
          let s1 = Group.get g 2 in
          Some ({ at with column = at.column + String.length s1 }, s1, ignore s0)
        | _ ->
          None
  }

let read ?(at = nowhere) f input =
  match (f ()).run (at, input) with
  | Some (_, _, res) ->
    Some res
  | None ->
    None
