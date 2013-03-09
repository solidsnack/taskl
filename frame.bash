#!/bin/bash
set -o errexit -o nounset -o pipefail
function -h {
cat <<EOF
 USAGE: $0
        $0 //<task> <arg>*
        $0 <internal function>

  In the first form, runs all the top-level tasks and their dependencies. The
  remaining forms are for debugging.

EOF
}; function --help { -h ;}


function tasks {
  local depth=0
  : # Replace with worker body.
}


################################### Running raw commands: helpers and reporting

tmp=/tmp/"$(printf 'taskl.%04x%04x.%d\n' $RANDOM $RANDOM $$)"
function tmp {
  trap "rm -rf $tmp" EXIT
  mkdir -p "$tmp"
  chmod =t,u=rxw,g=rxs "$tmp"
}

# Every action is run in a subshell with log tailing. If the action's
# "command" part is a URL then we use curlx to download and run the URL.
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


############################################## Managing and reporting task runs

# This portion of the code makes heavy use of "variable variables" to access
# state local to a particular run, in particular: argument vectors for
# individual calls and stored exit statuses. The $depth variable is also
# shared among them (and should be set to 0 by the caller).

# Print a message on entry to a task and set the indent level. If the task has
# already been run, this procedure will simply repeat the message that
# indicated how the task went the first time round.
function enter {
  depth=$(( $depth + 1 ))
  stateful_status "$@"
}

# Execute a task if it hasn't already been executed, storing its return code in
# a shared variable.
function try {
  local exit_ptr="exit_$1"
  local args_ptr="args_$1[@]"
  if ! [[ ${!exit_ptr:-} ]]
  then
    "${!args_ptr}" && local code=$? || local code=$?
    eval "$exit_ptr=$code"
  fi
}

# Report on the status of a task (but not if it's already been reported) and
# restore the indent level.
function leave {
  local stat_ptr="stat_$1"
  local exit_ptr="exit_$1"
  case "${!stat_ptr}" in
    '*') stateful_status "$@" ;;
  esac
  depth=$(( $depth - 1 ))
  return "${!exit_ptr}"
}

function stateful_status {
  local stat_ptr="stat_$1"
  local exit_ptr="exit_$1"
  local args_ptr="args_$1[@]"
  case "${!exit_ptr:-}" in
    '') eval "$stat_ptr='*'" ;;
    0)  eval "$stat_ptr='+'" ;;
    *)  eval "$stat_ptr='!'" ;;
  esac
  status_line "${!stat_ptr}" "$depth" "${!args_ptr}"
}


#################################################################### Formatting

function out { printf '%s\n' "$*" ;}
function msg { printf '%79s\n' "$*" >&2 ;}
function err { local x=$? ; msg "$*" ; return $(( $x == 0 ? 1 : $x )) ;}

function status_line {
  local status="$1" ; shift
  local indent=$(( 10#$1 )) ; shift
  printf "%-${depth}s" "$status"
  printf ' \0%s\0' "$@"
  echo
}

function bar {
  local s="$*"
  local n=$(( 72 - ${#s} ))
  printf ' ==%s %s ==\n' \
         "$( [[ $n -lt 1 ]] || yes = | head -n$n | tr -d -c = )" \
         "$s" >&2
}


############################################### Arguments and options, dispatch

if [[ $# -gt 0 ]]
then if [[ $1 ]] && declare -F | cut -d' ' -f3 | fgrep -qx -- "$1"
     then "$@"
     else err 'No such subcommand: `'"$1""'"
     fi
else tasks "$@" # Unrecognized so use default function.
fi

