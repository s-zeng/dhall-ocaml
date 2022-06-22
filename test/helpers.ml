open! Core

let whole_parse ~parser = Angstrom.parse_string ~consume:Angstrom.Consume.All parser

let test_parses
    (type t)
    (module T : Sexpable.S with type t = t)
    ~(parser : T.t Angstrom.t)
    ~successes
    ~failures
  =
  let successes = List.map ~f:(whole_parse ~parser) successes in
  let failures = List.map ~f:(whole_parse ~parser) failures in
  print_s [%message (successes : (T.t, string) Result.t list)];
  print_s [%message (failures : (T.t, string) Result.t list)]
;;

