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

let subset p =
  { fwd = (function x0 -> Option.some_if (p x0) x0);
    bwd = (function x0 -> Option.some_if (p x0) x0)
  }

let element ?(compare = Pervasives.compare) x0 =
  { fwd = (function x1 -> Option.some_if (compare x0 x1 = 0) ());
    bwd = (function () -> Some x0)
  }

let singleton =
  { fwd = (function x0 -> Some (x0 :: []));
    bwd = (function x0 :: [] -> Some x0 | _ -> None)
  }

let cons =
  { fwd = (function (x0, xs0) -> Some (x0 :: xs0));
    bwd = (function (x0 :: xs0) -> Some (x0, xs0) | _ -> None)
  }

let snoc =
  let init xs = List.take xs (List.length xs - 1) in
  let last xs = List.last_exn xs in
  { fwd = (function (xs0, x0) -> Some (xs0 @ [x0]));
    bwd = (function [] -> None | xs0 -> Some (init xs0, last xs0))
  }

let nil =
  { fwd = (function () -> Some []);
    bwd = (function [] -> Some () | _ -> None)
  }

let rec unfoldl ~f init acc =
  match f init with
  | Some (x0, x1) -> 
    unfoldl ~f x0 (x1 :: acc)
  | None ->
    (init, acc)

let rec unfoldr ~f init acc =
  match f init with
  | Some (x0, x1) -> 
    unfoldr ~f x1 (x0 :: acc)
  | None ->
    (List.rev acc, init)

let foldl ~f ~init:x0 xs0 =
  xs0 >>= List.fold_left ~init:x0 ~f:begin fun x y ->
    Option.both x (Some y) >>= f
  end

let foldr ~f ~init:x0 xs0 =
  xs0 >>= List.fold_right ~init:x0 ~f:begin fun x y -> 
    Option.both (Some x) y >>= f
  end

let foldl d0 =
  { fwd = (function (x0, xs0) -> foldl ~f:d0.fwd ~init:(Some x0) (Some xs0));
    bwd = (function xs0 -> unfoldl ~f:d0.bwd xs0 [] |> Option.some)
  }

let foldr d0 =
  { fwd = (function (xs0, x0) -> foldr ~f:d0.fwd ~init:(Some x0) (Some xs0));
    bwd = (function xs0 -> unfoldr ~f:d0.bwd xs0 [] |> Option.some)
  }

let fst d0 =
  { fwd = (function (x0, _) -> Some x0);
    bwd = (function x0 -> d0.bwd () >>| fun x1 -> (x0, x1))
  }

let snd d0 =
  { fwd = (function (_, x0) -> Some x0);
    bwd = (function x0 -> d0.bwd () >>| fun x1 -> (x1, x0))
  }

let some =
  { fwd = (function x0 -> Some (Some x0));
    bwd = (function Some x0 -> Some x0 | None -> None)
  }
  
let none =
  { fwd = (function _ -> Some None);
    bwd = (function Some _ -> None | None -> Some ())
  }

let string =
  { fwd = (function x0 -> Some (String.of_char_list x0));
    bwd = (function x0 -> Some (String.to_list x0))
  }

let integer =
  { fwd = (function x0 -> try Some (int_of_string x0) with Failure _ -> None);
    bwd = (function x0 -> try Some (string_of_int x0) with Failure _ -> None)
  }

let boolean =
  { fwd = (function x0 -> try Some (bool_of_string x0) with Invalid_argument _ -> None);
    bwd = (function x0 -> try Some (string_of_bool x0) with Invalid_argument _ -> None)
  }
