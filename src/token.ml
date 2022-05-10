(* Dhall.Parser.Token *)

open! Core
open Angstrom

let void f = f >>| Fn.const ()
let of_unicode_pred = Fn.flip Fn.compose Uchar.of_char
let endOfLine = string "\n" <|> string "\r\n" <?> "newline"
let validCodePoint _c = raise_s [%message "tbd"]

let lineCommentPrefix =
  let predicate c = Uchar.is_char c || [%equal: Uchar.t] c (Uchar.of_char '\t') in
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
    && (not ([%equal: Uchar.t] c (Uchar.of_char '-')))
    && not ([%equal: Uchar.t] c (Uchar.of_char '{')))
    || [%equal: Uchar.t] c (Uchar.of_char '\n')
    || [%equal: Uchar.t] c (Uchar.of_char '\t')
  in
  (* TODO: unicode *)
  let characters = take_while1 (of_unicode_pred characters_predicate) in
  let character_predicate c =
    Uchar.is_char c
    || [%equal: Uchar.t] c (Uchar.of_char '\n')
    || [%equal: Uchar.t] c (Uchar.of_char '\t')
  in
  let character = satisfy (of_unicode_pred character_predicate) >>| String.of_char in
  choice [ blockComment (); characters; character; endOfLine ]
;;

let blockCommentContinue = blockCommentContinue ()
let blockComment = blockComment ()
let blockCommentChunk = blockCommentChunk ()

let whitespaceChunk =
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
