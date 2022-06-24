open! Core

module type ApplicativeWithHigherKinded = sig
  type 'a t

  val return : 'a -> 'a t
  val map2 : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  include Higher_kinded.S with type 'a t := 'a t
end

let replicateM
    (type a higher_kinded)
    (module M : ApplicativeWithHigherKinded with type higher_kinded = higher_kinded)
    ~times
    (action : (a -> M.higher_kinded) Higher_kinded.t)
  =
  List.range 0 times
  |> List.map ~f:(Fn.const action)
  |> List.fold
       ~init:(M.inject (M.return []))
       ~f:(fun acc current ->
         M.inject (M.map2 ~f:List.cons (M.project current) (M.project acc)))
;;

(* taken from Dhall.Parser.Combinators.base *)
let int_of_char_list digits ~base =
  let snoc result number = (result * base) + number in
  let digitToNumber = function
    | c when Char.between c ~low:'0' ~high:'9' -> 0x0 + Char.to_int c - Char.to_int '0'
    | c when Char.between c ~low:'A' ~high:'F' -> 0xA + Char.to_int c - Char.to_int 'A'
    | c when Char.between c ~low:'a' ~high:'f' -> 0xa + Char.to_int c - Char.to_int 'a'
    | c -> raise_s [%message "Invalid hex digit" (c : char)]
  in
  List.fold ~f:snoc ~init:0 (List.map ~f:digitToNumber digits)
;;
