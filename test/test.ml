open OUnit2
open Postgresql_simple.Client
open Sys

module PG = Postgresql

let get_env_var var default () =
  try
    Sys.getenv(var)
  with Not_found ->
    default

let psql_conninfo =
  get_env_var "OCAML_PSQL_CONNINFO"
              "host=127.0.0.1 port=5432 dbname=ocaml_psql_test user=postgres password=postgres"

let string_bucket () =
  Random.self_init ();
  let number = Random.bits () in
  let bucket = ("ounit_" ^ string_of_int(number)) in
  bucket

let setup () =
  try
    let conninfo = psql_conninfo () in
    let conn = connect conninfo in
    conn
  with
    PG.Error err -> failwith (PG.string_of_error err)

let teardown conn =
  try
    ignore (disconnect conn)
  with
    PG.Error err -> failwith (PG.string_of_error err)

let db_fixture test_case test_ctxt =
  let conn = setup () in
  let _ = test_case test_ctxt conn in
  teardown conn

let table_fixture test_case test_ctxt =
  let table_name = string_bucket () in
  let create_q = Printf.sprintf
                   "CREATE TABLE %s (id serial PRIMARY KEY,
                                     num integer,
                                     data varchar,
                                     bool boolean
                    );" table_name in
  let drop_q = Printf.sprintf "DROP TABLE %s;" table_name in

  let conn = setup () in
  assert_bool "Cannot CREATE test table" (execute conn create_q = Result.Ok);
  let _ = test_case test_ctxt conn table_name in
  assert_bool "Cannot DROP test table" (execute conn drop_q = Result.Ok);
  teardown conn

let test_create_drop_table test_ctxt conn =
  let res = execute conn "CREATE TABLE test (id serial PRIMARY KEY, num integer, data varchar, bool boolean);" in
  assert_bool "Cannot CREATE table" (res = Result.Ok);
  let res = execute conn "DROP TABLE test;" in
  assert_bool "Cannot DROP table" (res = Result.Ok)

let test_null_values test_ctxt conn table_name  =
  let records = [("1", "NULL", "true");
                 ("NULL", "'asd'", "false");
                 ("3", "'NULL'", "true");
                 ("4", "''", "true")] in

  let insert (num, data, bool) =
    let q = Printf.sprintf "INSERT INTO %s (num, data, bool) VALUES (%s, %s, %s)" table_name num data bool in
    match execute conn q with
    | Result.Ok -> true
    | Result.Error err -> failwith err
    | Result.Tuples _ -> false
  in
  assert_bool "Not all records where inserted"
              (List.map insert records = [true; true; true; true]);

  let selected_records =
    let q = Printf.sprintf "SELECT num, data, bool FROM %s ORDER BY id" table_name in
    match execute conn q with
    | Result.Ok -> failwith "Unexpected Ok response"
    | Result.Error err -> failwith err
    | Result.Tuples l -> l
  in
  assert_bool "Row 1 is not expected" (List.nth selected_records 0 = [`Int 1; `NULL; `Bool true]);
  assert_bool "Row 2 is not expected" (List.nth selected_records 1 = [`NULL; `String "asd"; `Bool false]);
  assert_bool "Row 3 is not expected" (List.nth selected_records 2 = [`Int 3; `String "NULL"; `Bool true]);
  assert_bool "Row 4 is not expected" (List.nth selected_records 3 = [`Int 4; `String ""; `Bool true])

let test_point test_ctxt conn =
  let q = "SELECT '(1,2)'::point;" in
  match execute conn q with
  | Result.Ok -> failwith "Unexpected Ok response"
  | Result.Error err -> failwith err
  | Result.Tuples l -> assert_bool "Wrong point" (l = [[`Point (1.0, 2.0)]])

let _ =
  let suite = "Postgresql_simple" >::: [
    "test_create_drop_table" >:: (db_fixture test_create_drop_table);
    "test_null_values" >:: (table_fixture test_null_values);
    "test_point" >:: (db_fixture test_point);
  ] in
  run_test_tt_main suite
