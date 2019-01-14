(* 
 * Copyright (c) 2019 Takahisa Watanabe <takahisa.watanabe@outlook.com> All rights reserved.
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
open Base
open Base.Option

type ('a, 'b) t =
  { f: 'a -> 'b option;
    g: 'b -> 'a option
  }

let iso ~f ~g =
  { f = f;
    g = g
  }
let f { f; _ } = f
let g { g; _ } = g

let identity =
  { f = (fun x0 -> Some x0);
    g = (fun x0 -> Some x0)
  }

let compose d0 d1 =
  { f = (fun x0 -> d0.f x0 >>= d1.f);
    g = (fun x0 -> d1.g x0 >>= d0.g)
  }

let inverse d0 =
  { f = (fun x0 -> d0.g x0);
    g = (fun x0 -> d0.f x0)
  }

let commute =
  { f = (fun (x0, x1) -> Some (x1, x0));
    g = (fun (x0, x1) -> Some (x1, x0))
  }

let product d0 d1 =
  { f = (fun (x0, x1) -> Option.both (d0.f x0) (d1.f x1));
    g = (fun (x0, x1) -> Option.both (d0.g x0) (d1.g x1))
  }

let assoc =
  { f = (fun (x0, (x1, x2)) -> Some ((x0, x1), x2));
    g = (fun ((x0, x1), x2) -> Some (x0, (x1, x2)))
  }
