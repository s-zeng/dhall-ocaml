open! Core

type 'a t = 'a Angstrom.t

let return = Angstrom.return
let map2 ~f a b = Angstrom.(map ~f a <*> b)

include Higher_kinded.Make (Angstrom)
