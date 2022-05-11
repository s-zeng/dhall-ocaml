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
  let predicate c = Uchar.is_char c || [%equal: Uchar.t] c (uchar '\t') in
  (* TODO fix unicode handling *)
  let+ _ = string "--"
  and+ commentText = take_while (of_unicode_pred predicate) in
  "--" ^ commentText
;;

let lineComment =
  let _try = option "" in
  _try (lineCommentPrefix <* endOfLine)
;;

(* TODO check no infinite loop *)
let rec blockCommentContinue () =
  let endOfComment = void (string "-}") *> return "" in
  let continue =
    let+ c = blockCommentChunk ()
    and+ c' = blockCommentContinue () in
    c ^ c'
  in
  endOfComment <|> continue

and blockComment () =
  let+ _ = string "{-"
  and+ c = blockCommentContinue () in
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
  choice [ blockComment (); characters; character; endOfLine ]
;;

let blockCommentContinue = blockCommentContinue ()
let blockComment = blockComment ()
let blockCommentChunk = blockCommentChunk ()

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

let signPrefix =
  let positive = char '+' $> Fn.id in
  let negative = char '-' $> Int.neg in
  positive <|> negative <?> "sign"
;;
