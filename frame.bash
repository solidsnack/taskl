#!/bin/bash
set -o errexit -o nounset -o pipefail

function curl_sh {
  local url="$1" ; shift
  curl_ "$url" | sh -s -- "$@"
}

function curl_ {
  curl -sSfL --retry 4 "$1"
}

function msg { out "$*" 1>&2 ;}
function err { local x=$? ; msg "$*" ; return $(( $x == 0 ? 1 : $x )) ;}
function out { printf '%s\n' "$*" ;}

"$@"

