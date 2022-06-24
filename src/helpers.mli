open! Core

module type ApplicativeWithHigherKinded = sig
  type 'a t

  val return : 'a -> 'a t
  val map2 : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  include Higher_kinded.S with type 'a t := 'a t
end

module HigherKindedAngstrom : ApplicativeWithHigherKinded with type 'a t = 'a Angstrom.t

(** Haskell's Control.Applicative.replicateM *)
val replicateM
  :  (module ApplicativeWithHigherKinded with type higher_kinded = 'higher_kinded)
  -> times:int
  -> ('a -> 'higher_kinded) Higher_kinded.t
  -> ('a list -> 'higher_kinded) Higher_kinded.t

(** Convert a list of digits to the equivalent number *)
val int_of_char_list : char list -> base:int -> int
