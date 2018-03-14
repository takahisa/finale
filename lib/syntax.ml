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
include Syntax_intf

module Make (Pretty: PRETTY) (Parser: PARSER) = struct
  type 'a parser = 'a Parser.parser
  type 'a pretty = 'a Pretty.pretty
  type 'a syntax =
    { parse: unit -> 'a Parser.syntax;
      print: unit -> 'a Pretty.syntax 
    }

  let (<$>) d0 p0 =
    { parse = (fun () -> Parser.(d0 <$> p0.parse ()));
      print = (fun () -> Pretty.(d0 <$> p0.print ()))
    }
  let (<|>) p0 p1 =
    { parse = (fun () -> Parser.(p0.parse () <|> p1.parse ()));
      print = (fun () -> Pretty.(p0.print () <|> p1.print ()))
    }
  let (<*>) p0 p1 =
    { parse = (fun () -> Parser.(p0.parse () <*> p1.parse ()));
      print = (fun () -> Pretty.(p0.print () <*> p1.print ()))
    }
  let fix f =
    let rec parse = lazy (Parser.fix (fun parse -> (f { parse = (fun () -> parse); print = fix.print; }).parse ()))
        and print = lazy (Pretty.fix (fun print -> (f { print = (fun () -> print); parse = fix.parse; }).print ()))
        and fix =
          { parse = (fun () -> Lazy.force parse);
            print = (fun () -> Lazy.force print)
          }
    in fix
  let fail =
    { parse = (fun () -> Parser.fail);
      print = (fun () -> Pretty.fail)
    }

  let pure ?(compare = Pervasives.compare) x0 =
    { parse = (fun () -> Parser.pure ~compare x0);
      print = (fun () -> Pretty.pure ~compare x0)
    }

  let skip p0 =
    { parse = (fun () -> Parser.(skip @@ p0.parse ()));
      print = (fun () -> Pretty.(skip @@ p0.print ()))
    }

  let (~!) p0 =
    { parse = (fun () -> Parser.((~!) @@ p0.parse ()));
      print = (fun () -> Pretty.((~&) @@ p0.print ()))
    }
  let (~&) p0 =
    { parse = (fun () -> Parser.((~&) @@ p0.parse ()));
      print = (fun () -> Pretty.((~&) @@ p0.print ()))
    }

  let any =
    { parse = (fun () -> Parser.any);
      print = (fun () -> Pretty.any)
    }

  let print p = Pretty.print (p.print ())
  let parse p = Parser.parse (p.parse ())
  let apply p =
      { Iso.fwd = parse p;
        Iso.bwd = print p
      }
end
