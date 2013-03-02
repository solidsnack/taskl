#!/bin/bash
set -o errexit -o nounset -o pipefail

function tasks {
  : # Replace with worker body.
}

tmp=/tmp/"$(printf 'taskl.%04x%04x.%d\n' $RANDOM $RANDOM $$)"
function tmp {
  trap "rm -rf $tmp" EXIT
  mkdir -p "$tmp"
  chmod =t,u=rxw,g=rxs "$tmp"
}

function run {
  case "$1" in
    http://*|https://*) local cmd=( curlx "$@" ) ;;
    *)                  local cmd=( exec  "$@" ) ;;
  esac
  tmp
  if ( exec 1> >(exec tail -n20 > "$tmp"/o)
       exec 2> >(exec tail -n20 > "$tmp"/e)
       "${cmd[@]}" )
  then : # Do nothing.
  else
    local x=$?
    bar "Command failed with exit code $x"
    msg "$@"
    if [[ -s "$tmp"/o ]]
    then bar "Last few (up to 20) lines of stdout" && cat "$tmp"/o >&2
    else bar "No stdout available"
    fi
    if [[ -s "$tmp"/e ]]
    then bar "Last few (up to 20) lines of stderr" && cat "$tmp"/e >&2
    else bar "No stderr available"
    fi
    return $(( $x == 0 ? 1 : $x ))
  fi
}

function curlx {
  local url="$1" ; shift
  local bin="$tmp"/x
  curl -sSfL --retry 2 "$url" > "$bin"
  chmod u+rx "$bin"
  exec "$bin" "$@"
}

function out { printf '%s\n' "$*" ;}
function msg { printf '%79s\n' "$*" >&2 ;}
function err { local x=$? ; msg "$*" ; return $(( $x == 0 ? 1 : $x )) ;}

function bar {
  local s="$*"
  local n=$(( 72 - ${#s} ))
  printf ' ==%s %s ==\n' \
         "$( [[ $n -lt 1 ]] || yes = | head -n$n | tr -d -c = )" \
         "$s" >&2
}

if declare -F | cut -d' ' -f3 | fgrep -qx -- "${1:-}"
then "$@"       # Recognized function so call with arguments.
else tasks "$@" # Unrecognized so use default function.
fi

