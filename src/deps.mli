module DependencyResolver : sig
  type 'a t

  (* deps is a structure holding dependencies
     as class name - file name couples. *)
  val deps : 'a t ref

  exception DepsException of string

  (*  *)
  val find_in_deps : string -> string

  (* build goes through a (flat) directory and mutates deps. *)
  val build : string -> unit

  (* run takes a class name as argument and goes
     through dependencies and disambiguates them. *)
  (* val run : 'a -> bool *)

  (* show prints dependencies built from a directory. *)
  val show : unit -> unit
end
