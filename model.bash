#!/bin/bash

# Note that `-o' means enable while `+o' means disable.
set -o errexit
set -o nounset
set -o pipefail
set -o functrace
set -o errtrace
set -o noglob
set +o histexpand


# Peform the check for the given operation. Only atomic operations have
# checks.
function check {
  case "${1}" in
    $'fs/own:/usr/foo\nbar')
      echo "Would be test code."
      ;;
    $'fs/mode:/usr/foo\nbar')
      echo "Would be test code."
      ;;
    $'fs/obj:/usr/foo\nbar')
      echo "Would be test code."
      ;;
    $'pw/user:mo')
      echo "Would be test code."
      ;;
    *)
      echo 'Mysterious call to unknown task.' 1>&2
      exit 16
      ;;
  esac
}

function has {
  if [[ ${#1} -eq 0 ]]
  then
    echo true
    exit 0
  else
    echo false
    exit 8
  fi
}

function tasks {
  local task="$1"
  local operation="$2"
  local depth="$3"
  if ! [[ ${#task} -eq 0 ]]
  then
    $no_inmessage || printf "%${depth}s${in_symbol}%s\0\n" '' "${task}"
    let depth++
  fi
  case "${task}" in
    '')
      $operation '' $depth
      tasks $'fs:/usr/foo\nbar'
      tasks $'user:mo'
      ;;
    $'fs:/usr/foo\nbar')
      $operation "$task" $depth
      tasks $'fs/obj:/usr/foo\nbar'
      tasks $'fs/own:/usr/foo\nbar'
      tasks $'fs/mode:/usr/foo\nbar'
      ;;
    *)
    $'fs/own:/usr/foo\nbar')
      $operation "$task" $depth
      ;;
    $'fs/mode:/usr/foo\nbar')
      $operation "$task" $depth
      ;;
    $'fs/obj:/usr/foo\nbar')
      $operation "$task" $depth
      ;;
    $'pw/user:mo')
      $operation "$task" $depth
      ;;
    *)
      $operation '' $depth
      echo 'Mysterious call to unknown task.' 1>&2
      exit 16
      ;;
  esac
  if ! [[ ${#task} -eq 0 ]]
  then
    $no_outmessage || printf "%${depth}s << %s\0\n" '' "${task}"
    let depth--
  fi
}


mode=''
in_symbol=' >> '
no_outmessage=false
no_inmessage=false
depth=0
case "${1}" in
  install)
    mode=$1
    ;;
  check)
    mode=$1
    ;;
  list)
    mode=$1
    no_outmessage=true
    in_symbol=''
    ;;
  has)
    mode=$1
    no_outmessage=true
    no_inmessage=true
    ;;
  *)
    # Check if it's a number; that's how we'll communicate depth.
    depth=${1}
    shift
    ;;
esac

case "${2}" in
  '')
    ;;
esac

