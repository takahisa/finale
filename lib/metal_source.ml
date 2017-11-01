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
type position =
  { name : string;
    line : int;
    column : int }

let nowhere =
  { name = "<nowhere>";
    line = -1;
    column = -1 }

type 'a fragment =
  { it : 'a;
    at : position }

let it fragment = fragment.it
let at fragment = fragment.at 

let (@@@) it at =
  { it = it;
    at = at }

let info ?(at = nowhere) message =
  Printf.eprintf "[INFO] %s (at %d:%d)" message at.line at.column
    
let warn ?(at = nowhere) message =
  Printf.eprintf "[WARN] %s (at %d:%d)" message at.line at.column
    
let error ?(at = nowhere) message =
  failwith (Printf.sprintf "[ERROR] %s (at %d:%d)" message at.line at.column)
