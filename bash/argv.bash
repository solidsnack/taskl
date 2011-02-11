#!/bin/bash

function argv {
  printf '%s\x00' "$@"
}

function argvx {
  argv "$@" | xxd -p
}

