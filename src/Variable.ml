module StringHash =
    struct
        type t = String.t
        let equal = (=)
        let hash = Hashtbl.hash
    end

module VariableTable = Hashtbl.Make (StringHash)

type t = Calc.t

let vars = ref (VariableTable.create 200)

let add (s:string) (t:Calc.t) = VariableTable.add !vars s t
let get (s:string) = VariableTable.find !vars s
