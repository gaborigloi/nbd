(*
 * Copyright (C) Citrix Systems Inc.
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

open Lwt.Infix
open Protocol
open Channel

exception Client_requested_abort

type name = string

type t = {
  channel: channel;
  request: Cstruct.t; (* buffer used to read the request headers *)
  reply: Cstruct.t;   (* buffer used to write the response headers *)
  m: Lwt_mutex.t; (* prevents partial message interleaving *)
}

type size = int64

let close t = t.channel.close ()

let make channel =
  let request = Cstruct.create Request.sizeof in
  let reply = Cstruct.create Reply.sizeof in
  let m = Lwt_mutex.create () in
  { channel; request; reply; m }

let connect channel ?offer () =
  let section = Lwt_log_core.Section.make("Server.connect") in
  let%lwt () = Lwt_log_core.notice ~section "Starting fixed-newstyle negotiation" in

  let buf = Cstruct.create Announcement.sizeof in
  Announcement.(marshal buf `V2);
  let%lwt () = channel.write_clear buf in
  let buf = Cstruct.create (Negotiate.sizeof `V2) in
  Negotiate.(marshal buf (V2 [ GlobalFlag.Fixed_newstyle ]));
  let%lwt () = channel.write_clear buf in
  let buf = Cstruct.create NegotiateResponse.sizeof in
  let%lwt () = channel.read_clear buf in
  (* Option negotiation *)
  let req = Cstruct.create OptionRequestHeader.sizeof in
  let res = Cstruct.create OptionResponseHeader.sizeof in
  let make_blank_payload hdr =
    Cstruct.create (Int32.to_int hdr.OptionRequestHeader.length)
  in
  let respond ?(len=0) opt resp writefn =
    OptionResponseHeader.(marshal res { request_type = opt; response_type = resp; length = Int32.of_int len });
    writefn res
  in
  let send_ack opt writefn = respond opt OptionResponse.Ack writefn
  in
  let read_hdr_and_payload readfn =
    let%lwt () = readfn req in
    match OptionRequestHeader.unmarshal req with
    | `Error e -> Lwt.fail e
    | `Ok hdr ->
        let payload = make_blank_payload hdr in
        let%lwt () = readfn payload in
        Lwt.return (hdr.OptionRequestHeader.ty, payload)
  in
  let generic_loop chan =
    let rec loop () =
      let%lwt (opt, payload) = read_hdr_and_payload chan.read in
      match opt with
        | Option.StartTLS ->
          let resp = if chan.is_tls then OptionResponse.Invalid else OptionResponse.Policy in
          let%lwt () = respond opt resp chan.write in
          loop ()
        | Option.ExportName -> Lwt.return (Cstruct.to_string payload, make chan)
        | Option.Abort -> Lwt.fail Client_requested_abort
        | Option.Unknown _ ->
          let%lwt () = respond opt OptionResponse.Unsupported chan.write in
          loop ()
        | Option.List ->
          begin match offer with
            | None ->
              let%lwt () = respond opt OptionResponse.Policy chan.write in
              loop ()
            | Some offers ->
              let rec advertise = function
                | [] -> send_ack opt chan.write
                | x :: xs ->
                  let len = String.length x in
                  let%lwt () = respond ~len opt OptionResponse.Server chan.write in
                  let name = Cstruct.create len in
                  Cstruct.blit_from_string x 0 name 0 len;
                  let%lwt () = chan.write name in
                  advertise xs in
              let%lwt () = advertise offers in
              loop ()
          end
      in loop ()
  in
  let negotiate_tls make_tls_channel =
    let rec negotiate_tls () =
      let%lwt (opt, _) = read_hdr_and_payload channel.read_clear in
      match opt with
        | Option.ExportName -> Lwt.fail_with "Client requested export over cleartext channel but server is in FORCEDTLS mode."
        | Option.Abort -> Lwt.fail_with "Client requested abort (before negotiating TLS)."
        | Option.StartTLS -> (
            let%lwt () = send_ack opt channel.write_clear in
            let%lwt tch = make_tls_channel () in
            generic_loop (Channel.generic_of_tls_channel tch)
          )
        (* For any other option, respond saying TLS is required, then await next OptionRequest. *)
        | _ ->
          let%lwt () = respond opt OptionResponse.TlsReqd channel.write_clear in
          negotiate_tls ()
    in negotiate_tls ()
  in
  let client_flags = NegotiateResponse.unmarshal buf in
  (* Does the client support Fixed_newstyle? *)
  let old_client = not (List.mem ClientFlag.Fixed_newstyle client_flags) in
  match channel.make_tls_channel with
    | None -> (    (* We are in NOTLS mode *)
        let%lwt () =
          (if old_client
           then Lwt_log_core.warning ~section "Client doesn't report Fixed_newstyle"
           else Lwt.return_unit)
        in
        (* Continue regardless *)
        generic_loop (Channel.generic_of_cleartext_channel channel)
      )
    | Some make_tls_channel -> (   (* We are in FORCEDTLS mode *)
        if old_client
        then (
          let%lwt () = Lwt_log_core.error ~section "Server rejecting connection: it wants to use TLS but client flags don't include Fixed_newstyle" in
          Lwt.fail_with "client does not report Fixed_newstyle and server is in FORCEDTLS mode."
        )
        else negotiate_tls make_tls_channel
      )

let with_connection clearchan ?offer f =
  let%lwt (exportname, t) = connect clearchan ?offer () in
  (f exportname t) [%lwt.finally close t]

let negotiate_end t  size flags : t Lwt.t =
  let buf = Cstruct.create DiskInfo.sizeof in
  DiskInfo.(marshal buf { size; flags });
  let%lwt () = t.channel.write buf in
  Lwt.return { channel = t.channel; request = t.request; reply = t.reply; m = t.m }

let next t =
  let%lwt () = t.channel.read t.request in
  match Request.unmarshal t.request with
  | `Ok r -> Lwt.return r
  | `Error e -> Lwt.fail e

let ok t handle payload =
  Lwt_mutex.with_lock t.m
    (fun () ->
       Reply.marshal t.reply { Reply.handle; error = `Ok () };
       let%lwt () = t.channel.write t.reply in
       match payload with
       | None -> Lwt.return ()
       | Some data -> t.channel.write data
    )

let error t handle code =
  Lwt_mutex.with_lock t.m
    (fun () ->
       Reply.marshal t.reply { Reply.handle; error = `Error code };
       t.channel.write t.reply
    )

let serve t (type t) ?(read_only=true) block (b:t) =
  let section = Lwt_log_core.Section.make("Server.serve") in
  let module Block = (val block: V1_LWT.BLOCK with type t = t) in

  let%lwt () = Lwt_log_core.notice_f ~section "Serving new client, read_only = %b" read_only in

  let%lwt info = Block.get_info b in
  let size = Int64.(mul info.Block.size_sectors (of_int info.Block.sector_size)) in
  let%lwt read_only =
    (match read_only, info.Block.read_write with
     | true, _ -> Lwt.return true
     | false, true -> Lwt.return false
     | false, false ->
       let%lwt () = Lwt_log_core.error ~section "Read-write access was requested, but block is read-only, sending NBD_FLAG_READ_ONLY transmission flag" in
       Lwt.return true)
  in
  let flags = if read_only then [ PerExportFlag.Read_only ] else [] in
  let%lwt t = negotiate_end t size flags in

  let block = Io_page.(to_cstruct (get 128)) in
  let block_size = Cstruct.len block in
  let rec loop () =
    let%lwt request = next t in
    let open Request in
    match request with
    | { ty = Command.Write; from; len; handle } ->
      if read_only
      then error t handle `EPERM
      else if Int64.(rem from (of_int info.Block.sector_size)) <> 0L || Int64.(rem (of_int32 len) (of_int info.Block.sector_size) <> 0L)
      then error t handle `EINVAL
      else begin
        let rec copy offset remaining =
          let n = min block_size remaining in
          let subblock = Cstruct.sub block 0 n in
          let%lwt () = t.channel.Channel.read subblock in
          let%lwt res = Block.write b Int64.(div offset (of_int info.Block.sector_size)) [ subblock ] in
          match res with
          | `Error e ->
            let%lwt () = Lwt_log_core.debug_f ~section "Error while writing: %s; returning EIO error" (Block_error_printer.to_string e) in
            error t handle `EIO
          | `Ok () ->
            let remaining = remaining - n in
            if remaining > 0
            then copy Int64.(add offset (of_int n)) remaining
            else let%lwt () = ok t handle None in loop () in
        copy from (Int32.to_int request.Request.len)
      end
    | { ty = Command.Read; from; len; handle } ->
      (* It is okay to disconnect here in case of errors. The NBD protocol
         documentation says about NBD_CMD_READ:
         "If an error occurs, the server SHOULD set the appropriate error code
         in the error field. The server MAY then initiate a hard disconnect.
         If it chooses not to, it MUST NOT send any payload for this request.
         If an error occurs while reading after the server has already sent out
         the reply header with an error field set to zero (i.e., signalling no
         error), the server MUST immediately initiate a hard disconnect; it
         MUST NOT send any further data to the client." *)
      if Int64.(rem from (of_int info.Block.sector_size)) <> 0L || Int64.(rem (of_int32 len) (of_int info.Block.sector_size) <> 0L)
      then error t handle `EINVAL
      else begin
        let%lwt () = ok t handle None in
        let rec copy offset remaining =
          let n = min block_size remaining in
          let subblock = Cstruct.sub block 0 n in
          let%lwt res = Block.read b Int64.(div offset (of_int info.Block.sector_size)) [ subblock ] in
          match res with
          | `Error e ->
            Lwt.fail_with (Printf.sprintf "Partial failure during a Block.read: %s; terminating the session" (Block_error_printer.to_string e))
          | `Ok () ->
            let%lwt () = t.channel.write subblock in
            let remaining = remaining - n in
            if remaining > 0
            then copy Int64.(add offset (of_int n)) remaining
            else loop () in
        copy from (Int32.to_int request.Request.len)
      end
    | { ty = Command.Disc; _ } ->
      let%lwt () = Lwt_log.notice ~section "Received NBD_CMD_DISC, disconnecting" in
      Lwt.return_unit
    | _ ->
      let%lwt () = Lwt_log_core.warning ~section "Received unknown command, returning EINVAL" in
      error t request.Request.handle `EINVAL in
  loop ()
