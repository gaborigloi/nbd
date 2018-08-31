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

(** Tests that the client and server correctly implement the NBD protocol:
    https://github.com/NetworkBlockDevice/nbd/blob/master/doc/proto.md

    This module tests the core NBD library by verifying that the communication
    between the client and the server exactly matches the specified test
    sequences. *)

open Lwt.Infix

(** An Alcotest TESTABLE for the data transmissions in the test sequences *)
let transmission =
  let fmt =
    Fmt.of_to_string
      (function
       | `Server, x -> "Server: " ^ (String.escaped x)
       | `Client, x -> "Client: " ^ (String.escaped x))
  in
  Alcotest.testable fmt (=)

(* All the flags in the NBD protocol are in network byte order (big-endian) *)

let option_reply_magic_number = "\x00\x03\xe8\x89\x04\x55\x65\xa9"
let nbd_request_magic = "\x25\x60\x95\x13"
let nbd_reply_magic = "\x67\x44\x66\x98"

(** The client or server wanted to read and there is no more data from the
    other side. *)
exception Failed_to_read_empty_stream

(** [make_channel role test_sequence] creates a channel for use by the NBD library
    from a test sequence containing the expected communication between the
    client and the server. Reads and writes will verify that the communication
    matches exactly what is in [test_sequence], which is a list of data
    transmission tuples, each specifying whether the client or the server is
    sending the data, and the actual data sent. [role] specifies whether the
    client or the server will use the created channel, the other side will be
    simulated by taking the responses from [test_sequence]. *)
let make_channel role test_sequence =
  let next = ref test_sequence in
  let rec read buf =
    Lwt_io.printlf "Reading %d bytes" (Cstruct.len buf) >>= fun () ->
    (* Ignore reads and writes of length 0 and treat them as a no-op *)
    if Cstruct.len buf = 0 then Lwt.return_unit else
      match !next with
      | (source, x) :: rest ->
        if source = role then failwith "Tried to read but should have written";
        let available = min (Cstruct.len buf) (String.length x) in
        Cstruct.blit_from_string x 0 buf 0 available;
        next := if available = String.length x then rest else (source, (String.sub x available (String.length x - available))) :: rest;
        Lwt_io.printlf "Read: %s" (x |> String.escaped) >>= fun () ->
        Lwt_io.flush_all () >>= fun () -> (* Ensure all debug messages get logged *)
        let buf = Cstruct.shift buf available in
        if Cstruct.len buf = 0
        then Lwt.return ()
        else read buf
      | [] -> Lwt.fail Failed_to_read_empty_stream in
  let rec write buf =
    Lwt_io.printlf "Writing: %s" (buf |> Cstruct.to_string |> String.escaped) >>= fun () ->
    (* Ignore reads and writes of length 0 and treat them as a no-op *)
    if Cstruct.len buf = 0 then Lwt.return_unit else
      match !next with
      | (source, x) :: rest ->
        if source <> role then failwith "Tried to write but should have read";
        let available = min (Cstruct.len buf) (String.length x) in
        let written = String.sub (Cstruct.to_string buf) 0 available in
        let expected = String.sub x 0 available in
        Alcotest.(check (of_pp (Fmt.of_to_string String.escaped))) "Wrote expected data" expected written;
        Lwt_io.printlf "Wrote %s" (written |> String.escaped) >>= fun () ->
        Lwt_io.flush_all () >>= fun () -> (* Ensure all debug messages get logged *)
        next := if available = String.length x then rest else (source, (String.sub x available (String.length x - available))) :: rest;
        let buf = Cstruct.shift buf available in
        if Cstruct.len buf = 0
        then Lwt.return ()
        else write buf
      | [] ->
        Lwt.fail_with
          (Printf.sprintf
             "Tried to write %s but the stream was empty"
             (buf |> Cstruct.to_string |> String.escaped))
  in
  let close () = Lwt.return () in
  let assert_processed_complete_sequence () = Alcotest.(check (list transmission)) "did not process complete sequence" !next [] in
  (assert_processed_complete_sequence, (read, write, close))

let unimplemented _ = failwith "unimplemented"

(** Passes a channel for use by the NBD client to the given function, verifying
    that all communcation matches the given test sequence and that the complete
    sequence has been processed after the function returns.
    Returns a function that can be passed to create a Alcotest_lwt.test_case *)
let with_client_channel s f =
  fun _switch () ->
    let (assert_processed_complete_sequence, (read, write, close)) = make_channel `Client s in
    f Nbd.Channel.{read; read_nonblock=unimplemented; write; write_nonblock=unimplemented; close; is_tls=false} >>= fun () ->
    assert_processed_complete_sequence ();
    Lwt.return_unit

(** Passes a channel for use by the NBD server to the given function, verifying
    that all communcation matches the given test sequence and that the complete
    sequence has been processed after the function returns.
    Returns a function that can be passed to create a Alcotest_lwt.test_case *)
let with_server_channel s f =
  fun _switch () ->
    let (assert_processed_complete_sequence, (read, write, close)) = make_channel `Server s in
    f Nbd.Channel.{read_clear=read; read_clear_nonblock=unimplemented; write_clear=write; write_clear_nonblock=unimplemented; close_clear=close; make_tls_channel=None} >>= fun () ->
    assert_processed_complete_sequence ();
    Lwt.return_unit

module V2_negotiation = struct

  let v2_negotiation_start = [
    `Server, "NBDMAGIC";
    `Server, "IHAVEOPT";
    `Server, "\000\001"; (* handshake flags: NBD_FLAG_FIXED_NEWSTYLE *)
    `Client, "\000\000\000\001"; (* client flags: NBD_FLAG_C_FIXED_NEWSTYLE *)

    `Client, "IHAVEOPT";
    `Client, "\000\000\000\001"; (* NBD_OPT_EXPORT_NAME *)
    `Client, "\000\000\000\007"; (* length of export name *)
    `Client, "export1";
  ]

  (* The server only sends this extra data after Nbd.Server.connect when we call Nbd.Server.serve *)
  let v2_negotiation = v2_negotiation_start @ [
    `Server, "\000\000\000\000\001\000\000\000"; (* size *)
    `Server, "\000\001"; (* transmission flags: NBD_FLAG_HAS_FLAGS (bit 0) *)
    `Server, (String.make 124 '\000');
  ]

  let test_client =
    Alcotest_lwt.test_case
      "Perform a negotiation using the second version of the protocol from the
     client's side."
      `Quick
      (with_client_channel v2_negotiation (fun channel ->
           Nbd.Client.negotiate channel "export1"
           >>= fun _ ->
           Lwt.return ()
         ))

  let test_server =
    Alcotest_lwt.test_case
      "Perform a negotiation using the second version of the protocol from the
     server's side."
      `Quick
      (with_server_channel v2_negotiation_start (fun channel ->
           Nbd.Server.connect channel ()
           >|= fun (export_name, _svr) ->
           Alcotest.(check string) "The server did not receive the correct export name" "export1" export_name
         ))
end

module V2_abort = struct
  let sequence = [
    `Server, "NBDMAGIC";
    `Server, "IHAVEOPT";
    `Server, "\000\001"; (* handshake flags: NBD_FLAG_FIXED_NEWSTYLE *)
    `Client, "\000\000\000\001"; (* client flags: NBD_FLAG_C_FIXED_NEWSTYLE *)

    `Client, "IHAVEOPT";
    `Client, "\000\000\000\002"; (* NBD_OPT_ABORT *)
    `Client, "\000\000\000\000";

    `Server, option_reply_magic_number;
    `Server, "\000\000\000\002";
    `Server, "\000\000\000\001"; (* NBD_REP_ACK *)
    `Server, "\000\000\000\000";
  ]

  let test_server =
    Alcotest_lwt.test_case
      "Client connects then aborts"
      `Quick
      (with_server_channel sequence (fun channel ->
           Lwt.catch
             (fun () ->
                Nbd.Server.connect channel () >>= fun _ ->
                Alcotest.fail "Server should not enter transmission mode"
             )
             (function
               | Nbd.Server.Client_requested_abort -> Lwt.return_unit
               | e -> Lwt.fail e)
         ))
end

module V2_abort_without_ack = struct
  (** The NBD protocol says: "the server SHOULD gracefully handle the client
   * sending an NBD_OPT_ABORT and closing the connection without waiting for a
   * reply." *)

  let sequence = [
    `Server, "NBDMAGIC";
    `Server, "IHAVEOPT";
    `Server, "\000\001"; (* handshake flags: NBD_FLAG_FIXED_NEWSTYLE *)
    `Client, "\000\000\000\001"; (* client flags: NBD_FLAG_C_FIXED_NEWSTYLE *)

    `Client, "IHAVEOPT";
    `Client, "\000\000\000\002"; (* NBD_OPT_ABORT *)
    `Client, "\000\000\000\000";
  ]

  let test_server =
    Alcotest_lwt.test_case
      "Client connects then aborts without reading ack"
      `Quick
      (with_server_channel sequence (fun channel ->
           Lwt.catch
             (fun () ->
                Nbd.Server.connect channel () >>= fun _ ->
                Alcotest.fail "Server should not enter transmission mode"
             )
             (function
               | Nbd.Server.Client_requested_abort -> Lwt.return_unit
               | e -> Lwt.fail e)
         ))
end

module V2_list_export_disabled = struct

  let sequence = [
    `Server, "NBDMAGIC"; (* read *)
    `Server, "IHAVEOPT";
    `Server, "\000\001"; (* handshake flags: NBD_FLAG_FIXED_NEWSTYLE *)
    `Client, "\000\000\000\001"; (* client flags: NBD_FLAG_C_FIXED_NEWSTYLE *)

    `Client, "IHAVEOPT";
    `Client, "\000\000\000\003"; (* NBD_OPT_LIST *)
    `Client, "\000\000\000\000";

    `Server, option_reply_magic_number;
    `Server, "\000\000\000\003";
    `Server, "\128\000\000\002"; (* NBD_REP_ERR_POLICY *)
    `Server, "\000\000\000\000";

    `Client, "IHAVEOPT";
    `Client, "\000\000\000\002"; (* NBD_OPT_ABORT *)
    `Client, "\000\000\000\000";

    `Server, option_reply_magic_number;
    `Server, "\000\000\000\002";
    `Server, "\000\000\000\001"; (* NBD_REP_ACK *)
    `Server, "\000\000\000\000";
  ]

  let test_client =
    Alcotest_lwt.test_case
      "Check that if we request a list of exports and are denied, the error is
     reported properly."
      `Quick
      (with_client_channel sequence (fun channel ->
           Nbd.Client.list channel
           >>= function
           | Error `Policy ->
             Lwt.return ()
           | _ -> failwith "Expected to receive a Policy error"
         ))

  let test_server =
    Alcotest_lwt.test_case
      "Check that the server denies listing the exports, and the error is
     reported properly."
      `Quick
      (with_server_channel sequence (fun channel ->
           Lwt.catch
             (fun () ->
                Nbd.Server.connect channel () >>= fun _ ->
                Alcotest.fail "Server should not enter transmission mode"
             )
             (function
               | Nbd.Server.Client_requested_abort -> Lwt.return_unit
               | e -> Lwt.fail e)
         ))
end

module V2_no_ack_after_abort = struct
  (** After a NBD_OPT_LIST, the client sends an abort, but the server
   * disconnects without sending an ack. The NBD protocol says: "the client
   * SHOULD gracefully handle the server closing the connection after receiving
   * an NBD_OPT_ABORT without it sending a reply" *)

  let sequence = [
    `Server, "NBDMAGIC"; (* read *)
    `Server, "IHAVEOPT";
    `Server, "\000\001"; (* handshake flags: NBD_FLAG_FIXED_NEWSTYLE *)
    `Client, "\000\000\000\001"; (* client flags: NBD_FLAG_C_FIXED_NEWSTYLE *)

    `Client, "IHAVEOPT";
    `Client, "\000\000\000\003"; (* NBD_OPT_LIST *)
    `Client, "\000\000\000\000";

    `Server, option_reply_magic_number;
    `Server, "\000\000\000\003";
    `Server, "\128\000\000\002"; (* NBD_REP_ERR_POLICY *)
    `Server, "\000\000\000\000";

    `Client, "IHAVEOPT";
    `Client, "\000\000\000\002"; (* NBD_OPT_ABORT *)
    `Client, "\000\000\000\000";
  ]

  let test_client =
    Alcotest_lwt.test_case
      "Server denies listing exports, and disconnects after abort without sending ack"
      `Quick
      (with_client_channel sequence (fun channel ->
           Nbd.Client.list channel
           >>= function
           | Error `Policy ->
             Lwt.return ()
           | _ -> failwith "Expected to receive a Policy error"
         ))
end

module V2_list_export_success = struct
  let sequence = [
    `Server, "NBDMAGIC"; (* read *)
    `Server, "IHAVEOPT";
    `Server, "\000\001"; (* handshake flags: NBD_FLAG_FIXED_NEWSTYLE *)
    `Client, "\000\000\000\001"; (* client flags: NBD_FLAG_C_FIXED_NEWSTYLE *)

    `Client, "IHAVEOPT";
    `Client, "\000\000\000\003"; (* NBD_OPT_LIST *)
    `Client, "\000\000\000\000";

    `Server, option_reply_magic_number;
    `Server, "\000\000\000\003";
    `Server, "\000\000\000\002"; (* NBD_REP_SERVER *)
    `Server, "\000\000\000\011";
    `Server, "\000\000\000\007";
    `Server, "export1";

    `Server, option_reply_magic_number;
    `Server, "\000\000\000\003";
    `Server, "\000\000\000\002"; (* NBD_REP_SERVER *)
    `Server, "\000\000\000\011";
    `Server, "\000\000\000\007";
    `Server, "export2";

    `Server, option_reply_magic_number;
    `Server, "\000\000\000\003";
    `Server, "\000\000\000\001"; (* NBD_REP_ACK *)
    `Server, "\000\000\000\000";

    `Client, "IHAVEOPT";
    `Client, "\000\000\000\002"; (* NBD_OPT_ABORT *)
    `Client, "\000\000\000\000";

    `Server, option_reply_magic_number;
    `Server, "\000\000\000\002";
    `Server, "\000\000\000\001"; (* NBD_REP_ACK *)
    `Server, "\000\000\000\000";
  ]

  let test_client =
    Alcotest_lwt.test_case
      "Client requests a list of exports"
      `Quick
      (with_client_channel sequence (fun channel ->
           Nbd.Client.list channel >|= fun res ->
           Alcotest.(check (result (slist string String.compare) reject))
             "Returned correct export names"
             (Ok [ "export1"; "export2" ])
             res
         ))

  let test_server =
    Alcotest_lwt.test_case
      "Client requests a list of exports"
      `Quick
      (with_server_channel sequence (fun channel ->
           Lwt.catch
             (fun () ->
                Nbd.Server.connect ~offer:["export1";"export2"] channel () >>= fun _ ->
                Alcotest.fail "Server should not enter transmission mode"
             )
             (function
               | Nbd.Server.Client_requested_abort -> Lwt.return_unit
               | e -> Lwt.fail e)
         ))
end

module V2_list_export_extra_data = struct
  let sequence = [
    `Server, "NBDMAGIC"; (* read *)
    `Server, "IHAVEOPT";
    `Server, "\000\001"; (* handshake flags: NBD_FLAG_FIXED_NEWSTYLE *)
    `Client, "\000\000\000\001"; (* client flags: NBD_FLAG_C_FIXED_NEWSTYLE *)
    `Client, "IHAVEOPT";
    `Client, "\000\000\000\003"; (* NBD_OPT_LIST *)
    `Client, "\000\000\000\000";

    `Server, option_reply_magic_number;
    `Server, "\000\000\000\003";
    `Server, "\000\000\000\002"; (* NBD_REP_SERVER *)
    `Server, "\000\000\000\018";
    `Server, "\000\000\000\007";
     (* The NBD protocol allows for extra implementation-specific data after the export name *)
    `Server, "export2<extra>";

    `Server, option_reply_magic_number;
    `Server, "\000\000\000\003";
    `Server, "\000\000\000\001"; (* NBD_REP_ACK *)
    `Server, "\000\000\000\000";

    `Client, "IHAVEOPT";
    `Client, "\000\000\000\002"; (* NBD_OPT_ABORT *)
    `Client, "\000\000\000\000";

    `Server, option_reply_magic_number;
    `Server, "\000\000\000\002";
    `Server, "\000\000\000\001"; (* NBD_REP_ACK *)
    `Server, "\000\000\000\000";
  ]

  let test_client =
    Alcotest_lwt.test_case
      "List exports with extra data after export name"
      `Quick
      (with_client_channel sequence (fun channel ->
           Nbd.Client.list channel
           >>= function
           | Ok [ "export2" ] ->
             Lwt.return ()
           | _ -> failwith "Expected to receive a list of exports"
         ))
end

module V2_read_only_test = struct

  let test_block = (Cstruct.of_string "asdf")

  let sequence = [
    `Server, "NBDMAGIC";
    `Server, "IHAVEOPT";
    `Server, "\000\001"; (* handshake flags: NBD_FLAG_FIXED_NEWSTYLE *)
    `Client, "\000\000\000\001"; (* client flags: NBD_FLAG_C_FIXED_NEWSTYLE *)

    `Client, "IHAVEOPT";
    `Client, "\000\000\000\001"; (* NBD_OPT_EXPORT_NAME *)
    `Client, "\000\000\000\007"; (* length of export name *)
    `Client, "export1";

    `Server, "\000\000\000\000\000\000\000\004"; (* size: 4 bytes *)
    `Server, "\000\003"; (* transmission flags: NBD_FLAG_READ_ONLY (bit 1) + NBD_FLAG_HAS_FLAGS (bit 0) *)
    `Server, (String.make 124 '\000');
    (* Now we've entered transmission mode *)

    `Client, nbd_request_magic;
    `Client, "\000\000"; (* command flags *)
    `Client, "\000\000"; (* request type: NBD_CMD_READ *)
    `Client, "\000\000\000\000\000\000\000\000"; (* handle: 4 bytes *)
    `Client, "\000\000\000\000\000\000\000\001"; (* offset *)
    `Client, "\000\000\000\002"; (* length *)

    (* We're allowed to read from a read-only export *)
    `Server, nbd_reply_magic;
    `Server, "\000\000\000\000"; (* error: no error *)
    `Server, "\000\000\000\000\000\000\000\000"; (* handle *)
    `Server, "sd"; (* 2 bytes of data *)

    `Client, nbd_request_magic;
    `Client, "\000\000"; (* command flags *)
    `Client, "\000\001"; (* request type: NBD_CMD_WRITE *)
    `Client, "\000\000\000\000\000\000\000\001"; (* handle: 4 bytes *)
    `Client, "\000\000\000\000\000\000\000\000"; (* offset *)
    `Client, "\000\000\000\004"; (* length *)
    (* The server should probably return the EPERM error immediately, and not
       read any data associated with the write request, as the client should
       recognize the error before transmitting the data, just like for EINVAL,
       which is sent for unaligned requests. *)
    (*`Client, "nope"; (* 4 bytes of data *)*)

    (* However, we're not allowed to write to it *)
    `Server, nbd_reply_magic;
    `Server, "\000\000\000\001"; (* error: EPERM *)
    `Server, "\000\000\000\000\000\000\000\001"; (* handle *)

    (* TODO: currently the test fails with the below lines uncommented, because
       the server disconnects in case of write errors, but according to the NBD
       protocol it probably shouldn't, it should continue to process the
       client's requests *)
    (*
    `Client, nbd_request_magic;
    `Client, "\000\000"; (* command flags *)
    `Client, "\000\002"; (* request type: NBD_CMD_DISC *)
    `Client, "\000\000\000\000\000\000\000\002"; (* handle: 4 bytes *)
    `Client, "\000\000\000\000\000\000\000\000"; (* offset *)
    `Client, "\000\000\000\000"; (* length *)
    *)
  ]

  let server_test =
    Alcotest_lwt.test_case
      "Serve a read-only export and test that reads and writes are handled correctly."
      `Quick
      (with_server_channel sequence (fun channel ->
           Nbd.Server.connect channel ()
           >>= fun (export_name, svr) ->
           Alcotest.(check string) "The server did not receive the correct export name" "export1" export_name;
           Nbd.Server.serve svr ~read_only:true (module Cstruct_block.Block) test_block
         ))

end

module V2_write_test = struct

  let test_block = (Cstruct.of_string "asdf")

  let sequence = [
    `Server, "NBDMAGIC";
    `Server, "IHAVEOPT";
    `Server, "\000\001"; (* handshake flags: NBD_FLAG_FIXED_NEWSTYLE *)
    `Client, "\000\000\000\001"; (* client flags: NBD_FLAG_C_FIXED_NEWSTYLE *)

    `Client, "IHAVEOPT";
    `Client, "\000\000\000\001"; (* NBD_OPT_EXPORT_NAME *)
    `Client, "\000\000\000\007"; (* length of export name *)
    `Client, "export1";

    `Server, "\000\000\000\000\000\000\000\004"; (* size: 4 bytes *)
    `Server, "\000\001"; (* transmission flags: NBD_FLAG_HAS_FLAGS (bit 0) *)
    `Server, (String.make 124 '\000');
    (* Now we've entered transmission mode *)

    `Client, nbd_request_magic;
    `Client, "\000\000"; (* command flags *)
    `Client, "\000\001"; (* request type: NBD_CMD_WRITE *)
    `Client, "\000\000\000\000\000\000\000\001"; (* handle: 4 bytes *)
    `Client, "\000\000\000\000\000\000\000\002"; (* offset *)
    `Client, "\000\000\000\002"; (* length *)
    `Client, "12"; (* 2 bytes of data *)

    (* We're allowed to read from a read-only export *)
    `Server, nbd_reply_magic;
    `Server, "\000\000\000\000"; (* error: no error *)
    `Server, "\000\000\000\000\000\000\000\001"; (* handle *)

    `Client, nbd_request_magic;
    `Client, "\000\000"; (* command flags *)
    `Client, "\000\002"; (* request type: NBD_CMD_DISC *)
    `Client, "\000\000\000\000\000\000\000\002"; (* handle: 4 bytes *)
    `Client, "\000\000\000\000\000\000\000\000"; (* offset *)
    `Client, "\000\000\000\000"; (* length *)
  ]

  let server_test =
    Alcotest_lwt.test_case
      "Serve a read-write export and test that writes are handled correctly."
      `Quick
      (with_server_channel sequence (fun channel ->
           Nbd.Server.connect channel ()
           >>= fun (export_name, svr) ->
           Alcotest.(check string) "The server did not receive the correct export name" "export1" export_name;
           Nbd.Server.serve svr ~read_only:false (module Cstruct_block.Block) test_block
           >|= fun () ->
           Alcotest.(check string) "Data written by server"
             "as12"
             (Cstruct.to_string test_block)
         ))

end

let tests =
  "Nbd protocol tests",
  [ V2_negotiation.test_client
  ; V2_negotiation.test_server
  ; V2_abort.test_server
  ; V2_abort_without_ack.test_server
  ; V2_list_export_disabled.test_client
  ; V2_list_export_disabled.test_server
  ; V2_no_ack_after_abort.test_client
  ; V2_list_export_success.test_client
  ; V2_list_export_success.test_server
  ; V2_list_export_extra_data.test_client
  ; V2_read_only_test.server_test
  ; V2_write_test.server_test
  ]
