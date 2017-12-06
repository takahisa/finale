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

type 'a repr = { run: input:'a -> string option }

let ( <$> ) d0 p0 =
  { run = 
      fun ~input:b0 ->
        Option.concat_map (d0.bwd b0) ~f:(fun a0 -> p0.run ~input:a0)
  }

let ( <*> ) p0 p1 =
  { run = 
      fun ~input:(a0, b0) ->
        Option.map ~f:(fun (z0, z1) -> z0 ^ z1) @@
          Option.(p0.run ~input:a0 *** p1.run ~input:b0)
  }

let ( <|> ) p0 p1 =
  { run =
      fun ~input:a0 ->
        match p0.run ~input:a0 with
        | Some z0 ->
          Some z0
        | None ->
          p1.run ~input:a0
  }

let ( <* ) p0 p1 =
  { run =
      fun ~input:a0 ->
        Option.concat_map (p0.run ~input:a0) ~f:begin fun z0 ->
          Option.concat_map (p1.run ~input:()) ~f:begin fun z1 ->
            Some (z0 ^ z1)
          end
        end
  }

let ( *> ) p0 p1 =
  { run =
      fun ~input:a1 ->
        Option.concat_map (p0.run ~input:()) ~f:begin fun z0 ->
          Option.concat_map (p1.run ~input:a1) ~f:begin fun z1 ->
            Some (z0 ^ z1)
          end
        end
  }

let ( <$ ) p0 d0 =
  { run =
      fun ~input:() ->
        Option.concat_map (d0.fwd ()) ~f:(fun a0 -> p0.run ~input:a0)
  }

let ( $> ) p0 d0 =
  { run =
      fun ~input:a0 ->
        Option.concat_map (d0.bwd a0) ~f:(fun () -> p0.run ~input:a0)
  }

let hold f =
  let p = Lazy.from_fun f in
  { run = fun ~input:a0 -> (Lazy.force p).run ~input:a0 }

let fail =
  { run = fun ~input:_ -> None }

let succeed =
  { run = fun ~input:_ -> Some "" }

let rec rep p = function
  | [] ->
    Some ""
  | h :: t ->
    Option.concat_map (p.run ~input:h) ~f:begin fun z0 ->
      Option.concat_map (rep p t) ~f:begin fun z1 ->
        Some (z0 ^ z1)
      end
    end

let rec count n0 p0 =
  { run =
      fun ~input:list ->
        if (n0 < 0) then
          raise (Invalid_argument "count");
        match list with
        | h :: t when (n0 > 0) ->
          Option.concat_map (p0.run ~input:h) ~f:begin fun z0 ->
            Option.concat_map ((count (n0-1) p0).run ~input:t) ~f:begin fun z1 ->
              Some (z0 ^ z1)
            end
          end
        | [] when n0 = 0 ->
          Some ""
        | _ ->
          None
  }

let rep0 p0 = { run = fun ~input:as0 -> rep p0 as0 }
let rep1 p0 = { run = fun ~input:as0 -> rep p0 as0 }

let sep p0 p1 = function
  | [] ->
    Some ""
  | h :: t ->
    Option.concat_map (p1.run ~input:h) ~f:begin fun z0 ->
      Option.concat_map (rep (p0 *> p1) t) ~f:begin fun z1 ->
        Some (z0 ^ z1)
      end
    end

let sep0 p0 p1 = { run = fun ~input:as0 -> sep p0 p1 as0 }
let sep1 p0 p1 = { run = fun ~input:as0 -> sep p0 p1 as0 }

let sep_end p0 p1 =
  rep (p1 <* p0)

let sep_end0 p0 p1 = { run = fun ~input:as0 -> sep_end p0 p1 as0 }
let sep_end1 p0 p1 = { run = fun ~input:as0 -> sep_end p0 p1 as0 }

let rec chainl1 d0 p0 p1 =
  { run =
      fun ~input:a0 ->
        let open Option in
        match d0.bwd a0 with
        | Some (a1, a2) ->
          concat_map ((chainl1 d0 p0 p1).run ~input:a1) ~f:(fun z0 ->
            concat_map (p0.run ~input:()) ~f:(fun z1 ->
              concat_map (p1.run ~input:a2) ~f:(fun z2 ->
                Some (z0 ^ z1 ^ z2))))
        | None ->
          p1.run ~input:a0
  }
let rec chainr1 d0 p0 p1 =
  { run =
      fun ~input:a0 ->
        let open Option in
        match d0.bwd a0 with
        | Some (a1, a2) ->
          concat_map (p1.run ~input:a1) ~f:(fun z0 ->
            concat_map (p0.run ~input:()) ~f:(fun z1 ->
              concat_map ((chainr1 d0 p0 p1 <|> p1).run ~input:a2) ~f:(fun z2 ->
                Some (z0 ^ z1 ^ z2))))
        | None ->
          p1.run ~input:a0
  }

let spaces0 = { run = fun ~input:_ -> Some " " }
let spaces1 = { run = fun ~input:_ -> Some " " }

let between lp rp p0 =
  lp *> p0 <* rp

let char =
  { run = fun ~input:c -> Some (String.make 1 c) }

let lower =
  subset (fun c -> Char.lowercase_ascii c = c) <$>
    { run = fun ~input:c -> Some (String.make 1 c) }
let upper =
  subset (fun c -> Char.uppercase_ascii c = c) <$>
    { run = fun ~input:c -> Some (String.make 1 c) }
let digit =
  let _0 = Char.code '0' in
  let _9 = Char.code '9' in
  subset (fun c -> let n = Char.code c in (_0 <= n && n <= _9)) <$> char

let show: (unit -> 'a repr) -> 'a -> string option =
  fun f -> fun a0 -> (f ()).run ~input:a0
