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

type ('a, 'b) iso =
  { fwd: 'a -> 'b option;
    bwd: 'b -> 'a option;
  }
let iso ~fwd ~bwd =
  { fwd;
    bwd;
  }
let fwd d0 = d0.fwd
let bwd d0 = d0.bwd

let identity =
  { fwd = (function x0 -> Some x0);
    bwd = (function x0 -> Some x0);
  }

let compose d0 d1 =
  { fwd = (function x0 -> d0.fwd x0 >>= d1.fwd);
    bwd = (function x0 -> d1.bwd x0 >>= d0.bwd);
  }

let inverse d0 =
  { fwd = (function x0 -> d0.bwd x0);
    bwd = (function x0 -> d0.fwd x0);
  }

let commute =
  { fwd = (function (x0, x1) -> Some (x1, x0));
    bwd = (function (x0, x1) -> Some (x1, x0));
  }

let product d0 d1 =
  { fwd = (function (x0, x1) -> Option.both (d0.fwd x0) (d1.fwd x1));
    bwd = (function (x0, x1) -> Option.both (d0.bwd x0) (d1.bwd x1));
  }

let associate =
  { fwd = (function (x0, (x1, x2)) -> Some ((x0, x1), x2));
    bwd = (function ((x0, x1), x2) -> Some (x0, (x1, x2)));
  }
