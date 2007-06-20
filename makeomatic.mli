type cmd = 
    [ `Compile of string
    | `Pack of string * string
    | `Link of string * string ]

module type S = sig
  val packs: (string * string list) list
  val execs: (string * string list) list
  val ocaml: cmd -> native:bool -> string -> string
  val gcc: string -> string

  val rules: (string -> string option) list
end

module Make(X : S) : sig end



val ppopt: string -> string
val compile_glob: string -> (string -> bool)
