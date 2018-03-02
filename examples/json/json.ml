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
open Finale
open Finale.Iso
open Finale.Iso_partial

type json =
  | JArray   of json list 
  | JObject  of (json * json) list
  | JNumber  of int
  | JString  of string
  | JBoolean of bool

let jarrayE =
  { fwd = (function x0 -> Some (JArray x0));
    bwd = (function JArray x0 -> Some x0 | _ -> None)
  }

let jobjectE =
  { fwd = (function x0 -> Some (JObject x0));
    bwd = (function JObject x0 -> Some x0 | _ -> None)
  }

let jnumberE =
  { fwd = (function x0 -> Some (JNumber x0));
    bwd = (function JNumber x0 -> Some x0 | _ -> None)
  }

let jstringE =
  { fwd = (function x0 -> Some (JString x0));
    bwd = (function JString x0 -> Some x0 | _ -> None)
  }

let jbooleanE =
  { fwd = (function x0 -> Some (JBoolean x0));
    bwd = (function JBoolean x0 -> Some x0 | _ -> None)
  }    

module Json (Pretty: Syntax.PRETTY) (Parser: Syntax.PARSER) = struct
  module Combinator_base = Syntax.Make (Pretty) (Parser)
  module Combinator = Combinator.Make (Combinator_base)

  open Combinator_base
  open Combinator
  let spaces = (spaces1 <|> spaces0)
  let lbracket = char '['  <* spaces
  let rbracket = char ']'  <* spaces
  let lbrace   = char '{'  <* spaces
  let rbrace   = char '}'  <* spaces
  let colon    = char ':'  <* spaces
  let comma    = char ','  <* spaces
  let quote    = char '\"'

  let jstring _ =
    (jstringE <$> (between quote quote (string <$> rep0 (~!quote *> any))))
    <* spaces

  let jnumber _ =
    (jnumberE <$> ((compose string integer) <$> rep1 digit))
    <* spaces

  let jboolean _ =
        ((jbooleanE <$> (text "true"  *> pure true))
    <|> ((jbooleanE <$> (text "false" *> pure false))))
    <* spaces

  let jobject jvalue =
    (jobjectE <$> 
       (between lbrace rbrace 
          (sep_end_by0 ~delimiter:comma (jstring jvalue <*> (colon *> jvalue)))))
    <* spaces

  let jarray jvalue =
    (jarrayE <$> 
       (between lbracket rbracket
          (sep_end_by0 ~delimiter:comma jvalue)))
    <* spaces0

  let jvalue =
    fix @@ fun jvalue ->
      choice [
        jstring jvalue;
        jnumber jvalue;
        jboolean jvalue;
        jobject jvalue;
        jarray jvalue;
      ]
  let json = between spaces0 spaces0 jvalue

  let parse = Combinator_base.parse jvalue
  let print = Combinator_base.print jvalue
end

let _ =
  let module Json = Json (Pretty) (Parser) in
  let result =
    In_channel.input_line In_channel.stdin >>= fun z ->
    Json.parse z >>= fun j ->
    Json.print j >>= fun z ->
    return (j, z)
  in
  match result with
  | Some (_, z) ->
    print_endline z
  | None ->
    print_endline "Failure"

