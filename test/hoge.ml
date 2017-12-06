open Metal
open Metal.Iso

type 'a exp =
  | Int: int -> int exp
  | Add: (int exp * int exp) -> int exp
  | Sub: (int exp * int exp) -> int exp

let intE =
  { fwd = (function n -> Some (Int n));
    bwd = (function (Int n) -> Some n | _ -> None)
  }

let addE =
  { fwd = (function (e0, e1) -> Some (Add (e0, e1)));
    bwd = (function (Add (e0, e1)) -> Some (e0, e1) | _ -> None)
  }

let subE =
  { fwd = (function (e0, e1) -> Some (Sub (e0, e1)));
    bwd = (function (Sub (e0, e1)) -> Some (e0, e1) | _ -> None)
  }

let implode = fun cs ->
  String.concat "" @@ List.map (fun c -> String.make 1 c) cs

let explode = fun z ->
  let n = String.length z in
  let rec go j =
    if j < n then String.get z j :: go (j+1) else []
  in go 0


let number =
  { fwd = (function cs -> Some (int_of_string @@ implode cs));
    bwd = (function n  -> Some (explode @@ string_of_int n))
  }

module Syntax (M: META_SYNTAX) = struct
  open M
  let lp = char <$ element '('
  let rp = char <$ element ')'

  let rec exp () =
    (addE <$> between lp rp ((hold exp) <*> (char <$ element '+') *> (hold exp))) <|>
    (subE <$> between lp rp ((hold exp) <*> (char <$ element '-') *> (hold exp))) <|>
    (compose number intE <$> rep1 digit)
  let exp = exp ()
end

let _ = Parser.read (fun () -> let module M = Syntax(Parser) in M.exp) "((1+2)-3)"
let _ = Pretty.show (fun () -> let module M = Syntax(Pretty) in M.exp) (Sub (Add (Int 1, Int 2), Int 3))

