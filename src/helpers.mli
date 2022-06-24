open! Core

module type MonadWithHigherKinded = sig
  type 'a t

  include Monad.S with type 'a t := 'a t
  include Higher_kinded.S with type 'a t := 'a t
end

(* Haskell's Control.Monad.replicateM *)
val replicateM
  :  (module MonadWithHigherKinded with type higher_kinded = 'higher_kinded)
  -> times:int
  -> ('a -> 'higher_kinded) Higher_kinded.t
  -> ('a list -> 'higher_kinded) Higher_kinded.t
