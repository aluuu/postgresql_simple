module Value = struct
  type t = [ `Bool of bool
           | `Int of int
           | `String of string]
  let of_string ftype value =
    let open Postgresql in
    match ftype with
    | INT2 | INT8 | INT4 -> `Int (int_of_string value)
    | BOOL -> `Bool (value = "t")
    | TEXT| VARCHAR -> `String value
    | _ -> failwith "not implemented"
end

module Result = struct
  type t = Tuples of Value.t list list
         | Error of string
         | Ok
  let fetch_tuples result =
    let nfields = result#nfields in
    let ntuples = result#ntuples in
    let lst_ref = ref [] in
    for t = ntuples - 1 downto 0 do
      let tpl_ref = ref [] in
      for i = nfields - 1 downto 0 do
        let ftype = result#ftype i in
        let value = result#getvalue t i in
        tpl_ref := Value.of_string ftype value :: !tpl_ref
      done;
      lst_ref := !tpl_ref :: !lst_ref
    done;
    Tuples !lst_ref
end

module Connection = struct
  type t = Connected of Postgresql.connection
         | Disconnected
end

let connect conninfo =
  let open Connection in
  let module PG = Postgresql in
  let conn = new PG.connection ~conninfo:conninfo () in
  match conn#status with
  | PG.Connection_started | PG.Connection_made | PG.Connection_awaiting_response
  | PG.Connection_auth_ok | PG.Connection_ssl_startup | PG.Connection_setenv
  | PG.Ok -> Connected conn
  | PG.Bad -> Disconnected

let execute conn query =
  let open Connection in
  let module PG = Postgresql in
  match conn with
      | Connected conn ->
         (try
             let result = conn#exec query in
             (match result#status with
              | PG.Bad_response
              | PG.Nonfatal_error
              | PG.Fatal_error -> Result.Error result#error
              | PG.Empty_query -> Result.Error "Cannot execute empty query"
              | PG.Tuples_ok
              | PG.Single_tuple -> Result.fetch_tuples result
              | PG.Command_ok
              | PG.Copy_in
              | PG.Copy_out
              | PG.Copy_both -> Result.Ok)
           with PG.Error err -> Result.Error (PG.string_of_error err))
      | Disconnected -> Result.Error "Can't execute query with closed connection"

let disconnect conn =
  let open Connection in
  let module PG = Postgresql in
  match conn with
  | Disconnected -> Disconnected
  | Connected conn -> conn#finish; Disconnected
