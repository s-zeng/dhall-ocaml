open! Core

module type MonadWithHigherKinded = sig
  type 'a t

  include Monad.S with type 'a t := 'a t
  include Higher_kinded.S with type 'a t := 'a t
end

let replicateM
    (type a higher_kinded)
    (module M : MonadWithHigherKinded with type higher_kinded = higher_kinded)
    ~times
    (action : (a -> M.higher_kinded) Higher_kinded.t)
  =
  List.range 0 times
  |> List.map ~f:(Fn.const action)
  |> List.fold
       ~init:(M.inject (M.return []))
       ~f:(fun acc current ->
         M.inject
           (let%map.M current = M.project current
            and rest = M.project acc in
            current :: rest))
;;
