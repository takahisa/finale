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
open Iso
open Iso_partial

module Make (Syntax: Syntax_intf.S) = struct
  open Syntax
  let ( *>) p0 p1 = snd (element ()) <$> (p0 <*> p1)
  let ( <*) p0 p1 = fst (element ()) <$> (p0 <*> p1)

  let sequence ps = 
    List.fold_right ~f:(fun hd tl -> cons <$> (hd <*> tl)) ~init:(pure []) ps

  let choice ps =
    List.fold_right ~f:(<|>) ~init:fail ps

  let option p0 =
    (some <$> p0) <|> (pure None)

  let count n0 p0 =
    sequence @@ List.init n0 ~f:(Fn.const p0)
  let rep1 p0 =
    fix (fun fix -> cons <$> (p0 <*> (fix <|> pure [])))
  let rep0 p0 =
    rep1 p0 <|> pure []

  let sep_by1 ~delimiter:p0 p1 =
    cons <$> (p1 <*> rep0 (p0 *> p1))
  let sep_by0 ~delimiter:p0 p1 =
    sep_by1 p0 p1 <|> pure []

  let end_by1 ~delimiter:p0 p1 =
    rep1 (p1 <* p0)
  let end_by0 ~delimiter:p0 p1 =
    rep0 (p1 <* p0)

  let sep_end_by1 ~delimiter:p0 p1 =
    sep_by1 p0 p1 <* skip (option p0)
  let sep_end_by0 ~delimiter:p0 p1 =
    sep_by0 p0 p1 <* skip (option p0)

  let between p0 p1 p2 =
    p0 *> p2 <* p1

  let text z =
    let n = String.length z in
    compose string (element z) <$> count n any

  let lower = subset Char.is_lowercase <$> any
  let upper = subset Char.is_uppercase <$> any
  let digit = subset Char.is_digit <$> any
  let alpha = subset Char.is_alpha <$> any
  let space = skip (subset Char.is_whitespace <$> any)
  let spaces0 = skip (rep0 space)
  let spaces1 = ((element ' ' <$> any) *> skip (rep0 space)) <|> skip (rep1 space)
end
