module type MAKECALC =
    sig
        type t
        val zero1 : t
        val zero2 : t
        val add : t -> t -> t
        val sub : t -> t -> t
        val mul : t -> t -> t
        val div : t -> t -> t
        val modulo : t -> t -> t
    end

module INT =
    struct
        type t = int
        let zero1 = 0
        let zero2 = 1
        let add e1 e2 = e1 + e2
        let sub e1 e2 = e1 - e2
        let mul e1 e2 = e1 * e2
        let div e1 e2 = e1 / e2
        let modulo e1 e2 = e1 mod e2
    end

module FLOAT =
    struct
        type t = float
        let zero1 = 0.
        let zero2 = 1.
        let add e1 e2 = e1 +. e2
        let sub e1 e2 = e1 -. e2
        let mul e1 e2 = e1 *. e2
        let div e1 e2 = e1 /. e2
        let modulo e1 e2 = mod_float e1 e2
    end

module MakeCalc =
    functor (Monoid : MAKECALC) ->
        struct
            let zero1 = Monoid.zero1
            let zero2 = Monoid.zero2
            let add = Monoid.add
            let sub = Monoid.sub
            let mul = Monoid.mul
            let div = Monoid.div
            let modulo = Monoid.modulo
            let power x y =
                let rec aux acc i =
                    if i = y then acc
                    else aux (mul acc x) (i+1)
                in aux Monoid.zero2 0
            let fact x =
                let rec aux acc x =
                    if x = 0 then acc
                    else aux (acc + x) (x-1)
                in aux 0 x
        end

module Calc_int = MakeCalc(INT)
module Calc_float = MakeCalc(FLOAT)

module Basic =
    struct
        type t = Int of int
               | Float of float

        let to_string t = match t with
            | Int i -> string_of_int i
            | Float f -> string_of_float f

        let add t1 t2 = match t1, t2 with
            | Int i1    , Int i2     -> Int (Calc_int.add i1 i2)
            | Float f1  , Float f2   -> Float (Calc_float.add f1 f2)
            | Int i     , Float f
            | Float f   , Int i      -> Float (Calc_float.add (float_of_int i) f)

        let sub t1 t2 = match t1, t2 with
            | Int i1    , Int i2     -> Int (Calc_int.sub i1 i2)
            | Float f1  , Float f2   -> Float (Calc_float.sub f1 f2)
            | Int i     , Float f
            | Float f   , Int i      -> Float (Calc_float.sub (float_of_int i) f)

        let mul t1 t2 = match t1, t2 with
            | Int i1    , Int i2     -> Int (Calc_int.mul i1 i2)
            | Float f1  , Float f2   -> Float (Calc_float.mul f1 f2)
            | Int i     , Float f
            | Float f   , Int i      -> Float (Calc_float.mul (float_of_int i) f)

        let div t1 t2 = match t1, t2 with
            | Int i1    , Int i2     -> Int (Calc_int.div i1 i2)
            | Float f1  , Float f2   -> Float (Calc_float.div f1 f2)
            | Int i     , Float f
            | Float f   , Int i      -> Float (Calc_float.div (float_of_int i) f)

        let modulo t1 t2 = match t1, t2 with
            | Int i1    , Int i2     -> Int (Calc_int.modulo i1 i2)
            | Float f1  , Float f2   -> Float (Calc_float.modulo f1 f2)
            | Int i     , Float f
            | Float f   , Int i      -> Float (Calc_float.modulo (float_of_int i) f)

        let power t1 t2 = match t1, t2 with
            | Int i1    , Int i2     -> Int (Calc_int.power i1 i2)
            | Float f   , Int i      -> Float (Calc_float.power f i)
            | _ -> invalid_arg ("power: " ^ to_string t2)

    end

module IMG =
    struct
        type t = Basic.t * Basic.t
        let zero1 = (Basic.Int 0, Basic.Int 0)
        let zero2 = (Basic.Int 1, Basic.Int 0)
        let add e1 e2 = Basic.add (fst e1) (fst e2), Basic.add (snd e1) (snd e2)
        let sub e1 e2 = Basic.sub (fst e1) (fst e2), Basic.sub (snd e1) (snd e2)
        let mul e1 e2 = Basic.mul (fst e1) (fst e2), Basic.add (Basic.add (Basic.mul (fst e1) (snd e2)) (Basic.mul (snd e1) (fst e2))) (Basic.mul (snd e1) (snd e2))
        let div e1 e2 = Basic.div (fst e1) (fst e2), Basic.add (Basic.add (Basic.div (fst e1) (snd e2)) (Basic.div (snd e1) (fst e2))) (Basic.div (snd e1) (snd e2))
        let modulo e1 e2 = Basic.modulo (fst e1) (fst e2), Basic.add (Basic.add (Basic.modulo (fst e1) (snd e2)) (Basic.modulo (snd e1) (fst e2))) (Basic.modulo (snd e1) (snd e2))
    end

module Calc_complex = MakeCalc(IMG)

module Calc =
    struct
        type t = Simple of Basic.t
               | Img of IMG.t

        let to_string t = match t with
            | Simple t -> Basic.to_string t
            | Img t -> let t1, t2 = t in Printf.sprintf "%s + %si" (Basic.to_string t1) (Basic.to_string t2)

        let add t1 t2 = match t1, t2 with
            | Simple n1, Simple n2 -> Simple (Basic.add n1 n2)
            | Img n1, Img n2 -> Img (Calc_complex.add n1 n2)
            | Simple n1, Img n2
            | Img n2, Simple n1 -> Img (Calc_complex.add ((n1, snd Calc_complex.zero1)) n2)

        let sub t1 t2 = match t1, t2 with
            | Simple n1, Simple n2 -> Simple (Basic.sub n1 n2)
            | Img n1, Img n2 -> Img (Calc_complex.sub n1 n2)
            | Simple n1, Img n2
            | Img n2, Simple n1 -> Img (Calc_complex.sub ((n1, snd Calc_complex.zero1)) n2)

        let mul t1 t2 = match t1, t2 with
            | Simple n1, Simple n2 -> Simple (Basic.mul n1 n2)
            | Img n1, Img n2 -> Img (Calc_complex.mul n1 n2)
            | Simple n1, Img n2
            | Img n2, Simple n1 -> Img (Calc_complex.mul ((n1, snd Calc_complex.zero2)) n2)

        let div t1 t2 = match t1, t2 with
            | Simple n1, Simple n2 -> Simple (Basic.div n1 n2)
            | Img n1, Img n2 -> Img (Calc_complex.div n1 n2)
            | Simple n1, Img n2
            | Img n2, Simple n1 -> Img (Calc_complex.div ((n1, snd Calc_complex.zero2)) n2)

        let modulo t1 t2 = match t1, t2 with
            | Simple n1, Simple n2 -> Simple (Basic.modulo n1 n2)
            | Img n1, Img n2 -> Img (Calc_complex.modulo n1 n2)
            | Simple n1, Img n2
            | Img n2, Simple n1 -> Img (Calc_complex.modulo ((n1, snd Calc_complex.zero2)) n2)

        let power t1 t2 = match t1, t2 with
            | Simple n1, Simple n2 -> Simple (Basic.power n1 n2)
            | Img n1, Simple (Basic.Int n2) -> Img (Calc_complex.power n1 n2)
            | _ -> invalid_arg ("Calc.power: " ^ to_string t2)

    end

module StringHash =
    struct
        type t = String.t
        let equal = (=)
        let hash = Hashtbl.hash
    end

module VariableTable = Hashtbl.Make (StringHash)

module Variables =
    struct
        type t = Calc.t

        let vars = ref (VariableTable.create 200)

        let add (s:string) (t:Calc.t) = VariableTable.add !vars s t
        let get (s:string) = VariableTable.find !vars s
    end

