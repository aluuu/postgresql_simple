type connection = Connected of Postgresql.connection
                | Disconnected

type result = Tuples of string list list
            | Error of string
            | Ok

let connect conninfo =
  let module PG = Postgresql in
  let conn = new PG.connection ~conninfo:conninfo () in
  match conn#status with
  | PG.Connection_started | PG.Connection_made | PG.Connection_awaiting_response
  | PG.Connection_auth_ok | PG.Connection_ssl_startup | PG.Connection_setenv
  | PG.Ok -> Connected conn
  | PG.Bad -> Disconnected

let execute conn query =
  let module PG = Postgresql in
  match conn with
      | Connected conn ->
         (try
             let result = conn#exec query in
             (match result#status with
              | PG.Bad_response
              | PG.Nonfatal_error
              | PG.Fatal_error -> Error result#error
              | PG.Empty_query -> Error "Cannot execute empty query"
              | PG.Tuples_ok
              | PG.Single_tuple -> Tuples result#get_all_lst
              | PG.Command_ok
              | PG.Copy_in
              | PG.Copy_out
              | PG.Copy_both -> Ok)
           with PG.Error err -> Error (PG.string_of_error err))
      | Disconnected -> Error "Can't execute query with closed connection"

let disconnect conn =
  let module PG = Postgresql in
  match conn with
  | Disconnected -> Disconnected
  | Connected conn -> conn#finish; Disconnected
