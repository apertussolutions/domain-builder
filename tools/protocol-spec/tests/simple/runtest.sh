#!/bin/bash
set -e

COMPILE="../../../dist/build/protocol-spec/protocol-spec"

CC=gcc
SPEC="../input.h"
DECODE_C="decode.c"
DECODE_H="decode.h"
ENCODE_C="encode.c"
ENCODE_H="encode.h"
PRETTY_C="pretty.c"
PRETTY_H="pretty.h"
TYPES_H="types.h"

TEST_OUT=test-output

mkdir -p $TEST_OUT
pushd $TEST_OUT > /dev/null

$COMPILE $SPEC $TYPES_H \
  --pi=$PRETTY_C --ph=$PRETTY_H \
  --ei=$ENCODE_C --eh=$ENCODE_H \
  --di=$DECODE_C --dh=$DECODE_H \
  -i .



$CC -I. -std=gnu99 -o main ../main.c $PRETTY_C $ENCODE_C $DECODE_C

./main

popd > /dev/null
# rm -rf $TEST_OUT
