module DependencyResolver : sig
  type 'a t

  (* deps is a structure holding dependencies
     as class name - file name couples. *)
  val deps : 'a t ref

  (* build goes through a (flat) directory and mutates deps. *)
  val build : string -> unit
end
