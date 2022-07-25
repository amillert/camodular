open Sem

module Env : sig
  type 'a t

  (* env is a running dependency environment
     to disambiguate the class scope. *)
  val env : 'a t ref

  (* findInEnv searches for a derived class in env *)
  val findInEnv : string -> scopeT

  (* mutateEnv either injects the class scope
     in env or updates already defined. *)
  val mutateEnv : classT -> unit
end
