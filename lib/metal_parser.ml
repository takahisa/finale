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
open Metal_aux

type 'a repr = { run: input:string -> (string * 'a) option }

let ( <$> ) d0 p0 =
  { run =
      fun ~input:z0 ->
        Option.concat_map (p0.run ~input:z0) ~f:begin fun (z1, a1) ->
          Option.concat_map (d0.fwd a1) ~f:begin fun b1 ->
            Some (z1, b1)
          end
        end
  }

let ( <*> ) p0 p1 =
  { run =
      fun ~input:z0 ->
        Option.concat_map (p0.run ~input:z0) ~f:begin fun (z1, a1) ->
          Option.concat_map (p1.run ~input:z1) ~f:begin fun (z2, a2) ->
            Some (z2, (a1, a2))
          end
        end
  }

let ( <|> ) p0 p1 =
  { run =
      fun ~input:z0 ->
        match p0.run ~input:z0 with
        | Some (z1, a1) ->
          Some (z1, a1)
        | None ->
          p1.run ~input:z0
  }

let ( <* ) p0 p1 =
  { run =
      fun ~input:z0 ->
        Option.concat_map (p0.run ~input:z0) ~f:begin fun (z1, a1) ->
          Option.concat_map (p1.run ~input:z1) ~f:begin fun (z2, _) ->
            Some (z2, a1)
          end
        end
  }

let ( *> ) p0 p1 =
  { run =
      fun ~input:z0 ->
        Option.concat_map (p0.run ~input:z0) ~f:begin fun (z1, _) ->
          Option.concat_map (p1.run ~input:z1) ~f:begin fun (z2, a2) ->
            Some (z2, a2)
          end
        end
  }

let ( <$ ) p0 d0 =
  { run =
      fun ~input:z0 ->
        Option.concat_map (p0.run ~input:z0) ~f:begin fun (z1, a1) ->          
          Option.concat_map (d0.bwd a1) ~f:begin fun () ->
            Some (z1, ())
          end
        end
  }

let ( $> ) p0 d0 =
  { run =
      fun ~input:z0 ->
        Option.concat_map (p0.run ~input:z0) ~f:begin fun (z1, a1) ->          
          Option.concat_map (d0.bwd a1) ~f:begin fun () ->
            Some (z1, a1)
          end
        end
  }

let hold f =
  let p = Lazy.from_fun f in
  { run = fun ~input:z0 -> (Lazy.force p).run ~input:z0 }

let fail =
  { run = fun ~input:z0 -> None }

let succeed =
  { run = fun ~input:z0 -> Some (z0, ()) }

let rec choice = function
  | h :: t ->
    h <|> choice t
  | [] ->
    fail

let rec count n0 p0 =
  { run =
      fun ~input:z0 ->
        if n0 > 0 then
          Option.concat_map (p0.run ~input:z0) ~f:begin fun (z1, h) ->
            Option.concat_map ((count (n0-1) p0).run ~input:z1) ~f:begin fun (z2, t) ->
              Some (z2, h :: t)
            end
          end
        else if n0 = 0 then
          Some (z0, [])
        else
          raise (Invalid_argument "count")
  }

let rec rep0 p0 =
  { run =
      fun ~input:z0 ->
        Option.(
          Option.concat_map (p0.run ~input:z0) ~f:begin fun (z1, h) ->
            Option.concat_map ((rep0 p0).run ~input:z1) ~f:begin fun (z2, t) ->
              Some (z2, h :: t)
            end
          end /// Some (z0, [])
        )
  }

let rep1 p0 =
  { run =
      fun ~input:z0 ->
        Option.map ((p0 <*> rep0 p0).run ~input:z0) ~f:(fun (z1, (h, t)) -> (z1, h :: t))
  }

let sep1 p0 p1 =
  { run =
      fun ~input:z0 ->
        Option.map ((p1 <*> rep0 (p0 *> p1)).run ~input:z0) ~f:(fun (z1, (h, t)) -> (z1, h :: t))
  }

let sep0 p0 p1 =
  { run =
      fun ~input:z0 ->
        Option.((sep1 p0 p1).run ~input:z0 /// Some (z0, []))
  }

let sep_end0 p0 p1 =
  rep0 (p1 <* p0)

let sep_end1 p0 p1 =
  rep1 (p1 <* p0)

let rec chainl1 d0 p0 p1 =
  { run =
      fun ~input:z0 ->
        let open Option in
        concat_map (p1.run ~input:z0) ~f:begin fun (z1, a1) ->
          (chainl1_rest d0 p0 p1 a1).run ~input:z1
        end
  }
and chainl1_rest d0 p0 p1 a0 =
  { run =
      fun ~input:z0 ->
        let open Option in
        concat_map ((p0 *> p1).run ~input:z0) ~f:begin fun (z1, a1) ->
          concat_map (d0.fwd (a0, a1)) ~f:begin fun a2 ->
            (chainl1_rest d0 p0 p1 a2).run ~input:z1
          end
        end /// (Some (z0, a0))
  }
  
let rec chainr1 d0 p0 p1 =
  { run =
      fun ~input:z0 ->
        let open Option in
        concat_map (p1.run ~input:z0) ~f:begin fun (z1, a1) ->
          (chainr1_rest d0 p0 p1 a1).run ~input:z1
        end
  }
and chainr1_rest d0 p0 p1 a0 =
  { run =
      fun ~input:z0 ->
        let open Option in
        concat_map ((p0 *> chainr1 d0 p0 p1).run ~input:z0) ~f:begin fun (z1, a1) ->
          concat_map (d0.fwd (a0, a1)) ~f:begin fun a2 ->
            Some (z1, a2)
          end
        end /// (Some (z0, a0))
    }

let between lp rp p0 =
  lp *> p0 <* rp

let text: string -> string repr = fun z0 ->
  { run =
      fun ~input:z1 ->
        let n0 = String.length z0 in
        let n1 = String.length z1 in
        if n1 >= n0 && String.sub z1 0 n0 = z0 then
          Some (String.sub z1 n0 (n1 - n0), z0)
        else
          None
  }
        
let char: char repr =
  { run =
      fun ~input:z0 ->
        let length = String.length z0 in
        if (length > 0) then
          let z1 = String.sub z0 1 (length-1) in
          let c1 = String.get z0 0 in
          Some (z1, c1)
        else
          None
  }

let whitespace = choice @@ 
  List.map (fun c -> char <$ element c) [' '; '\t'; '\r'; '\n']
let spaces0 = { run = fun ~input:z0 -> Option.map ((rep0 whitespace).run ~input:z0) ~f:(fun (z1, _) -> (z1, ())) }
let spaces1 = { run = fun ~input:z0 -> Option.map ((rep1 whitespace).run ~input:z0) ~f:(fun (z1, _) -> (z1, ())) }

let _a = Char.code 'a'
let _z = Char.code 'z'
let lower =
  subset begin fun c -> 
    let code = Char.code c in
    _a <= code && code <= _z
  end <$> char

let _A = Char.code 'A'
let _Z = Char.code 'Z'
let upper =
  subset begin fun c -> 
    let code = Char.code c in
    _A <= code && code <= _Z
  end <$> char

let _0 = Char.code '0'
let _9 = Char.code '9'
let digit =
  subset begin fun c ->
    let code = Char.code c in
    _0 <= code && code <= _9
  end <$> char

let read: (unit -> 'a repr) -> string -> 'a option =
  fun f -> fun z0 -> Option.map ((f ()).run ~input:z0) ~f:(fun (_, a0) -> a0)
