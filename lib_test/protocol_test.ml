(*
 * Copyright (C) 2015 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open OUnit
open Nbd
open Lwt

(* All the flags in the NBD protocol are in network byte order (big-endian) *)

let v2_list_export_success = [
  `Server, "NBDMAGIC"; (* read *)
  `Server, "IHAVEOPT";
  `Server, "\000\001"; (* handshake flags: NBD_FLAG_FIXED_NEWSTYLE *)
  `Client, "\000\000\000\001"; (* client flags: NBD_FLAG_C_FIXED_NEWSTYLE *)
  `Client, "IHAVEOPT";
  `Client, "\000\000\000\003"; (* NBD_OPT_LIST *)
  `Client, "\000\000\000\000";

  `Server, "\x00\x03\xe8\x89\x04\x55\x65\xa9";
  `Server, "\000\000\000\003";
  `Server, "\000\000\000\002"; (* NBD_REP_SERVER *)
  `Server, "\000\000\000\011";
  `Server, "\000\000\000\007";
  `Server, "export1";

  `Server, "\x00\x03\xe8\x89\x04\x55\x65\xa9";
  `Server, "\000\000\000\003";
  `Server, "\000\000\000\001"; (* NBD_REP_ACK *)
  `Server, "\000\000\000\000";
]

let make_channel role test_sequence =
  let next = ref test_sequence in
  let rec read buf =
    (* Ignore reads and writes of length 0 and treat them as a no-op *)
    if Cstruct.len buf = 0 then Lwt.return_unit else
    match !next with
    | (source, x) :: rest ->
      if source = role then failwith "Tried to read but should have written";
      let available = min (Cstruct.len buf) (String.length x) in
      let bytes_read = String.sub x 0 available in
      Lwt_io.printl (String.escaped bytes_read) >>= fun () ->
      Cstruct.blit_from_string x 0 buf 0 available;
      next := if available = String.length x then rest else (source, (String.sub x available (String.length x - available))) :: rest;
      let buf = Cstruct.shift buf available in
      if Cstruct.len buf = 0
      then return ()
      else read buf
    | [] -> fail (Failure "Tried to read but the stream was empty") in
  let rec write buf =
    (* Ignore reads and writes of length 0 and treat them as a no-op *)
    if Cstruct.len buf = 0 then Lwt.return_unit else
    match !next with
    | (source, x) :: rest ->
      if source <> role then failwith "Tried to write but should have read";
      let available = min (Cstruct.len buf) (String.length x) in
      let written = String.sub (Cstruct.to_string buf) 0 available in
      Lwt_io.printl (String.escaped written) >>= fun () ->
      let expected = String.sub x 0 available in
      OUnit.assert_equal ~msg:(Printf.sprintf "wrote bytes: '%s' (length: %d), expected: '%s' (length: %d)" (String.escaped written) (String.length written) (String.escaped expected) (String.length expected)) expected written;
      next := if available = String.length x then rest else (source, (String.sub x available (String.length x - available))) :: rest;
      let buf = Cstruct.shift buf available in
      if Cstruct.len buf = 0
      then return ()
      else write buf
    | [] -> fail (Failure "Tried to write but the stream was empty") in
  let close () = return () in
  Channel.{ read_clear=read; write_clear=write; close_clear=close; make_tls_channel=None }

let make_client_channel s = make_channel `Client s
let make_server_channel s = make_channel `Server s

module V2_negotiation = struct

  let v2_negotiation = [
    `Server, "NBDMAGIC";
    `Server, "IHAVEOPT";
    `Server, "\000\001"; (* handshake flags: NBD_FLAG_FIXED_NEWSTYLE *)
    `Client, "\000\000\000\001"; (* client flags: NBD_FLAG_C_FIXED_NEWSTYLE *)
    `Client, "IHAVEOPT";
    `Client, "\000\000\000\001"; (* NBD_OPT_EXPORT_NAME *)
    `Client, "\000\000\000\007";
    `Client, "export1";
    `Server, "\000\000\000\000\001\000\000\000"; (* size *)
    `Server, "\000\000"; (* transmission flags *)
    `Server, (String.make 124 '\000');
  ]

  let client_negotiation =
    "Perform a negotiation using the second version of the protocol from the
     client's side."
    >:: fun () ->
      let t =
        let channel = make_client_channel v2_negotiation in
        Client.negotiate channel "export1"
        >>= fun (t, size, flags) ->
        return () in
      Lwt_main.run t

  let server_negotiation =
    "Perform a negotiation using the second version of the protocol from the
     server's side."
    >:: fun () ->
      let t =
        let channel = make_server_channel v2_negotiation in
        Server.connect channel ()
        >>= fun (export_name, svr) ->
        OUnit.assert_equal ~msg:"The server did not receive the correct export name" "export1" export_name;
        Lwt.return_unit
      in
      Lwt_main.run t
end

module V2_list_export_disabled = struct

  let v2_list_export_disabled = [
    `Server, "NBDMAGIC"; (* read *)
    `Server, "IHAVEOPT";
    `Server, "\000\001"; (* handshake flags: NBD_FLAG_FIXED_NEWSTYLE *)
    `Client, "\000\000\000\001"; (* client flags: NBD_FLAG_C_FIXED_NEWSTYLE *)
    `Client, "IHAVEOPT";
    `Client, "\000\000\000\003"; (* NBD_OPT_LIST *)
    `Client, "\000\000\000\000";
    `Server, "\x00\x03\xe8\x89\x04\x55\x65\xa9";
    `Server, "\000\000\000\003";
    `Server, "\128\000\000\002";
    `Server, "\000\000\000\000";
  ]

  let client_list_disabled =
    "Check that if we request a list of exports and are denied, the error is
     reported properly."
    >:: fun () ->
      let t =
        let channel = make_client_channel v2_list_export_disabled in
        Client.list channel
        >>= function
        | `Error `Policy ->
          return ()
        | _ -> failwith "Expected to receive a Policy error" in
      Lwt_main.run t

  let server_list_disabled =
    "Check that the server denies listing the exports, and the error is
     reported properly."
    >:: fun () ->
      let t =
        let channel = make_server_channel v2_list_export_disabled in
        Server.connect channel ()
        >>= fun (_export_name, _svr) ->
        Lwt.return_unit
      in
      Lwt_main.run t
end

  let list_success =
    "Check that if we request a list of exports, a list is returned and parsed
     properly."
    >:: fun () ->
      let t =
        let channel = make_client_channel v2_list_export_success in
        Client.list channel
        >>= function
        | `Ok [ "export1" ] ->
          return ()
        | _ -> failwith "Expected to receive a list of exports" in
      Lwt_main.run t

let tests =
  "Nbd client tests" >::: [
    V2_negotiation.client_negotiation;
    V2_negotiation.server_negotiation;
    V2_list_export_disabled.client_list_disabled;
    V2_list_export_disabled.server_list_disabled;
    list_success;
  ]
