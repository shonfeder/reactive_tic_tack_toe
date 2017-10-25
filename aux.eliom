[%%shared.start]

let (%) f g = fun x -> f (g x)
let id x = x
let rec range : int -> int -> int list
  = fun n m -> if n > m then [] else n :: range (n + 1) m

module Option = struct
  let is_some = function
    | Some _ -> true
    | None   -> false
end

module Monad = struct

  module type T = sig
    type 'a t
    val return : 'a  -> 'a t
    val bind   : 'a t -> ('a -> 'b t) -> 'b t
  end

  module Make (M:T) = struct

    type 'a t = 'a M.t

    let return = M.return
    let (>>=)  = M.bind

    let map
      : ('a -> 'b) -> 'a t -> 'b t
      = fun f m -> m >>= (return % f)

    let rec sequence
      : ('a -> 'b t) -> 'a t list -> 'b list t
      = fun f -> function
        | []    -> return []
        | x::xs -> x >>= f >>= fun x' -> map (List.cons x') (sequence f xs)
  end

  module type T2 = sig
    type ('a, 'e) t
    val return : 'a -> ('a, 'e) t
    val bind   : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  end

  module Make2 (M:T2) = struct

    type ('a, 'e) t = ('a, 'e) M.t

    let return = M.return
    let (>>=)  = M.bind

    let map
      : ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t
      = fun f m -> m >>= (return % f)

    let rec sequence
      : ('a -> ('b, 'e) t) -> ('a, 'e) t list -> ('b list, 'e) t
      = fun f -> function
        | []    -> return []
        | x::xs -> x >>= f >>= fun x' -> map (List.cons x') (sequence f xs)
  end

  module Base = struct
    module Option : T with type 'a t = 'a option = struct

      type 'a t = 'a option

      let return a = Some a

      let bind = function
        | Some a -> fun f -> f a
        | None   -> fun _ -> None
    end

    module Result : T2 with type ('a, 'e) t = ('a, 'e) result = struct
      type ('a, 'e) t = ('a, 'e) result
      let return a = Ok a
      let bind = function
        | Ok a    -> fun f -> f a
        | Error e -> fun _ -> Error e
    end

  end

  module Option = Make (Base.Option)
  module Result = Make2 (Base.Result)
end
