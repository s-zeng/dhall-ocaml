(* Dhall.Parser.Token *)

open! Core
open Angstrom

let ( $> ) f x = f >>| Fn.const x
let void f = f $> ()
let of_unicode_pred = Fn.flip Fn.compose Uchar.of_char
let uchar = Uchar.of_char
let endOfLine = string "\n" <|> string "\r\n" <?> "newline"
let validCodePoint _c = raise_s [%message "tbd"]

let lineCommentPrefix =
  let predicate c =
    Uchar.between c ~low:(Uchar.of_char '\x20') ~high:(Uchar.of_scalar_exn 0x10FFFF)
    || [%equal: Uchar.t] c (Uchar.of_char '\t')
  in
  (* TODO fix unicode handling *)
  let+ _ = string "--"
  and+ commentText = take_while (of_unicode_pred predicate) in
  "--" ^ commentText
;;

let lineComment =
  let _try = Fn.id in
  _try (lineCommentPrefix <* endOfLine)
;;

let of_lazy value = bind (return ()) ~f:value

let rec blockCommentContinue () =
  let endOfComment = void (string "-}") *> return "" in
  let continue =
    let+ c = of_lazy blockCommentChunk
    and+ c' = of_lazy blockCommentContinue in
    c ^ c'
  in
  endOfComment <|> continue

and blockComment () =
  let+ _ = string "{-"
  and+ c = of_lazy blockCommentContinue in
  "{-" ^ c ^ "-}"

and blockCommentChunk () =
  let characters_predicate c =
    (Uchar.is_char c
    && (not ([%equal: Uchar.t] c (uchar '-')))
    && not ([%equal: Uchar.t] c (uchar '{')))
    || [%equal: Uchar.t] c (uchar '\n')
    || [%equal: Uchar.t] c (uchar '\t')
  in
  let characters = take_while1 (of_unicode_pred characters_predicate) in
  let character_predicate c =
    Uchar.is_char c
    || [%equal: Uchar.t] c (uchar '\n')
    || [%equal: Uchar.t] c (uchar '\t')
  in
  let character = satisfy (of_unicode_pred character_predicate) >>| String.of_char in
  choice [ of_lazy blockComment; characters; character; endOfLine ]
;;

(* let blockCommentContinue = blockCommentContinue () *)
let blockComment = of_lazy blockComment
(* let blockCommentChunk = blockCommentChunk () *)

let whitespaceChunk =
  (* TODO: unicode *)
  let predicate = function
    | ' ' | '\t' | '\n' -> true
    | _ -> false
  in
  choice
    [ void (take_while1 predicate)
    ; void (string "\r\n" <?> "newline")
    ; void lineComment
    ; void blockComment
    ]
  <?> "whitespace"
;;

let whitespace = skip_many whitespaceChunk
let nonemptyWhitespace = skip_many1 whitespaceChunk
let alpha = Char.is_alpha
let digit = Char.is_digit
let alphaNum c = alpha c || digit c

let hexdig c =
  Char.between c ~low:'0' ~high:'9'
  || Char.between c ~low:'A' ~high:'F'
  || Char.between c ~low:'a' ~high:'f'
;;

module type Negatable = sig
  type t

  val neg : t -> t
end

let signPrefix (type t) (module N : Negatable with type t = t) =
  let positive = char '+' $> Fn.id in
  let negative = char '-' $> N.neg in
  positive <|> negative <?> "sign"
;;

let doubleLiteral =
  let fraction =
    let+ _ = char '.'
    and+ digits = many1 Primitive_tokens.digit in
    let snoc (y : Scientific.t) d =
      Scientific.(
        y
        + create
            ~coefficient:(Bigint.of_int (Int.of_string (String.of_char d)))
            ~exponent:(exponent y - 1))
    in
    List.fold_left ~f:snoc ~init:Scientific.zero digits
  in
  let exponent' =
    let+ _ = Primitive_tokens.oneOf [ 'e'; 'E' ]
    and+ sign = signPrefix (module Bigint) <|> return Fn.id
    and+ x = Primitive_tokens.decimal in
    Scientific.create ~coefficient:Bigint.one ~exponent:(Bigint.to_int_exn (sign x))
    (* TODO: to_int_exn is not the right behaviour here, check haskell's fromInteger :: Integer -> Int *)
  in
  (let* sign = signPrefix (module Float) <|> return Fn.id in
   let* x = Primitive_tokens.decimal in
   let alternative0 =
     let+ y = fraction
     and+ e = exponent' <|> return Scientific.one in
     Scientific.((Scientific.of_bigint x + y) * e)
   in
   let alternative1 =
     let+ expo = exponent' in
     Scientific.(of_bigint x * expo)
   in
   let+ n = alternative0 <|> alternative1 in
   sign (Scientific.toRealFloat n))
  <?> "literal"
;;
