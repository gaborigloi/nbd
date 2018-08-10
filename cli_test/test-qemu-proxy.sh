#!/bin/sh

set -eux

CLI=$1

SCRATCH=$(mktemp -d)
EXPORT=$SCRATCH/test
SOCKET=$SCRATCH/socket
OUTPUT=$SCRATCH/out

echo "**** Create a sparse qcow2 file ****"
qemu-img create -f qcow2 "$EXPORT" 40M

echo "**** Serve it with qemu-nbd ****"
qemu-nbd --export-name qemu_node --socket "$SOCKET" "$EXPORT" &
QEMU_SERVER=$!
echo "**** Wait for the server to start ****"
sleep 0.1

stop_qemu_server() {
  kill -9 $QEMU_SERVER
}
trap stop_qemu_server EXIT

echo "**** Proxy it ****"
$CLI proxy --exportname test --no-tls "nbd:unix:$SOCKET:exportname=qemu_node" &
SERVER=$!
echo "**** Wait for the server to start ****"
sleep 0.1

stop_servers() {
  kill -9 $SERVER
  stop_qemu_server
}
trap stop_servers EXIT

echo "**** Download it as raw from the server ****"
qemu-img convert 'nbd:0.0.0.0:10809:exportname=test' -O raw "$OUTPUT"
echo "**** Check that the two files are the same ****"
cmp --silent "$EXPORT" "$OUTPUT"

echo "**** Download it as qcow2 from the server - qemu will use structured reads if possible ****"
QCOW2=$SCRATCH/qcow2
qemu-img convert 'nbd:0.0.0.0:10809:exportname=test' -O qcow2 "$QCOW2"
echo "**** Convert the qcow2 to raw ****"
rm -f "$OUTPUT"
qemu-img convert "$QCOW2" "$OUTPUT"
echo "**** Check that the two files are the same ****"
cmp --silent "$EXPORT" "$OUTPUT"
echo "**** Check that the qcow2 is sparse ****"
[ "$(stat -c%s "$QCOW2")" -lt 40000000 ]
