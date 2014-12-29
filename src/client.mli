module Value: sig
  type t = [ `Bool of bool
           | `Int of int
           | `Float of float
           | `String of string
           | `Binary of string
           | `Inet of string
           | `Cidr of string
           | `Macaddr of string
           | `Void]
  val of_string: Postgresql.ftype -> string -> t
end

module Result: sig
  type t = Tuples of Value.t list list
         | Error of string
         | Ok
  val fetch_tuples: Postgresql.result -> t
end

module Connection: sig
  type t = Connected of Postgresql.connection
         | Disconnected
end

val connect: string -> Connection.t

val execute: Connection.t -> string -> Result.t

val disconnect: Connection.t -> Connection.t
