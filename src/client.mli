type connection = Connected of Postgresql.connection
                | Disconnected

type result = Tuples of string list list
            | Error of string
            | Ok

val connect: string -> connection

val execute: connection -> string -> result

val disconnect: connection -> connection
