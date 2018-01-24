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
type ('a, 'b) iso =
  { fwd: 'a -> 'b option;
    bwd: 'b -> 'a option; }

let iso f g =
  { fwd = f;
    bwd = g }

let fwd: ('a, 'b) iso -> 'a -> 'b option =
  fun d0 -> d0.fwd
let bwd: ('a, 'b) iso -> 'b -> 'a option =
  fun d0 -> d0.bwd

let identity =
  { fwd = (fun a0 -> Some a0);
    bwd = (fun a0 -> Some a0);
  }
  
let compose d0 d1 =
  { fwd = (fun a0 -> match d0.fwd a0 with Some b0 -> d1.fwd b0 | _ -> None);
    bwd = (fun c0 -> match d1.bwd c0 with Some b0 -> d0.bwd b0 | _ -> None);
  }
  
let inverse d0 =
  { fwd = (fun a0 -> d0.bwd a0);
    bwd = (fun b0 -> d0.fwd b0);
  }

let commute: ('a * 'b, 'b * 'a) iso =
  { fwd = (fun (a0, b0) -> Some (b0, a0));
    bwd = (fun (b0, a0) -> Some (a0, b0))
  }

let element ?(comparator = Pervasives.(=)) a0 =
  { fwd = (function () -> Some a0);
    bwd = (function a1 when comparator a0 a1 -> Some () | _ -> None)
  }

let subset f =
  { fwd = (function a0 when f a0 -> Some a0 | _ -> None);
    bwd = (function a0 when f a0 -> Some a0 | _ -> None);
  }

let list =
  { fwd = (function (hl, tl) -> Some (hl :: tl));
    bwd = (function (hl :: tl) -> Some (hl, tl) | _ -> None);
  }

let implode = fun cs ->
  String.concat "" @@ List.map (fun c -> String.make 1 c) cs

let explode = fun z ->
  let n = String.length z in
  let rec go j =
    if j < n then String.get z j :: go (j+1) else []
  in go 0

let string =
  { fwd = (function a -> Some (implode a));
    bwd = (function b -> Some (explode b))
  }

let number =
  { fwd = (function a -> try Some (int_of_string a) with _ -> None);
    bwd = (function b -> try Some (string_of_int b) with _ -> None)
  }
