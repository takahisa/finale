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
  | JArray  of json list 
  | JObject of (json * json) list
  | JNumber of int
  | JString of string
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
  module S = Syntax.Make (Pretty) (Parser)
  module C = Combinator.Make (S)
  include S
  include C

  let whitespace = choice (List.map ~f:(fun c -> element c <$> char) [' '; '\t'; '\r'; '\n'])
  let spaces0 = skip (rep0 whitespace)
  let spaces1 = whitespace *> spaces0

  let lbracket = (element '[' <$> char) <* spaces0
  let rbracket = (element ']' <$> char) <* spaces0
  let lbrace = (element '{' <$> char) <* spaces0
  let rbrace = (element '}' <$> char) <* spaces0
  let colon = (element ':'  <$> char) <* spaces0
  let comma = (element ','  <$> char) <* spaces0
  let quote = (element '"'  <$> char)

  let text z =
    let n = String.length z in
    (compose string (element z) <$> count n char) <* spaces0
  
  let jstring _ =
    jstringE <$> 
      (between quote quote
         (string <$> rep0 (subset ((<>) '"') <$> char)))
    <* spaces0

  let jnumber _ =
    jnumberE <$>
      ((compose string integer) <$> rep1 digit)
    <* spaces0

  let jboolean _ =
        ((jbooleanE <$> (text "true"  *> pure true))
    <|> ((jbooleanE <$> (text "false" *> pure false))))
    <* spaces0

  let jobject jvalue =
    jobjectE <$> 
      (between lbrace rbrace 
         (sep_end_by0 ~delimiter:comma (jstring jvalue <*> (colon *> jvalue))))
    <* spaces0

  let jarray jvalue =
    jarrayE <$> 
      (between lbracket rbracket
         (sep_end_by0 ~delimiter:comma jvalue))
    <* spaces0

  let jvalue =
    fix @@ fun jvalue ->
      choice [
        jstring jvalue;
        jnumber jvalue;
        jboolean jvalue;
        jobject jvalue;
        jarray jvalue
      ]
  let json = between spaces0 spaces0 jvalue
end

let _ =
  let module Json = Json (Pretty) (Parser) in
  let result =
    In_channel.input_line In_channel.stdin >>= fun line ->
    Json.parse Json.json line >>= fun json ->
    Json.print Json.json json >>= fun text ->
    return (json, text) in
  match result with
  | Some (_, print_result) ->
    Printf.printf "Success: %s\n" print_result
  | _ ->
    print_endline "Failure"
