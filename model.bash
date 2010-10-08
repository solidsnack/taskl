#!/bin/bash

# Note that `-o' means enable while `+o' means disable.
set -o errexit
set -o nounset
set -o pipefail
set -o functrace
set -o errtrace
set -o noglob
set +o histexpand

ERR_UNKNOWN_TASK=16
ERR_BAD_ARGS=8

operation='Task\L command never set; what gives?'
depth=0
task=''
waiting=false


function notify {
  printf "%${depth}s${1}\\0%s\\0\\n" '' "$2"
  $waiting && receive
}


# Code for checks, messages and actions. Only atomic actions are found in
# these case statements.

function checks {
  case "$1" in
    $'fs/ownership:/usr/foo\nbar')
      echo "Would be test code."
      ;;
    $'fs/mode:/usr/foo\nbar')
      echo "Would be test code."
      ;;
    $'fs/layout:/usr/foo\nbar')
      echo "Would be test code."
      ;;
    $'pw/user:mo')
      echo "Would be test code."
      ;;
    *)
      echo 'Mysterious call to unknown task.' 1>&2
      exit $ERR_UNKNOWN_TASK
      ;;
  esac
}

function messages {
  case "$1" in
    $'fs/ownership:/usr/foo\nbar')
      notify ' ** ' "Marking change to ownership."
      ;;
    $'fs/mode:/usr/foo\nbar')
      notify ' ** ' "Marking change to mode."
      ;;
    $'fs/layout:/usr/foo\nbar')
      notify ' ** ' "Marking change for file contents."
      ;;
    $'pw/user:mo')
      notify ' ** ' "Marking change for user's passwd DB entry."
      ;;
    *)
      echo 'Mysterious call to unknown task.' 1>&2
      exit $ERR_UNKNOWN_TASK
      ;;
  esac
}

function actions {
  case "$1" in
    $'fs/ownership:/usr/foo\nbar')
      echo "Run code here."
      ;;
    $'fs/mode:/usr/foo\nbar')
      echo "Run code here."
      ;;
    $'fs/layout:/usr/foo\nbar')
      echo "Run code here."
      ;;
    $'pw/user:mo')
      echo "Run code here."
      ;;
    *)
      echo 'Mysterious call to unknown task.' 1>&2
      exit $ERR_UNKNOWN_TASK
      ;;
  esac
}


# The operations. All of them rely on `branch' to route them through the
# package hierarchy.

function check {
  notify ' >> ' "$task"
  let depth++
  branch "$task"
  let depth--
}

function install {
  notify ' >> ' "$task"
  let depth++
  branch "$task"
  let depth--
  notify ' << ' "$task"
}

function list {
  notify ' ' "$task"
  branch "$task"
}

function has {
  if branch "$task"
  then
    echo true
    exit 0
  else
    echo false
    exit $ERR_UNKNOWN_TASK
  fi
}


# Leaf and branch, together, allow packages to be resolved to their atomic
# actions.

function leaf {
  case "$operation" in
    has|list)
      return 0
      ;;
    check|install)
      if already_handled "$task"
      then
        ${marked["$task"]} && notify ' ** ' 'Previously marked for change.'
      else
        if ! checks "$task"
        then
          messages "$task"
          [ "$operation" = 'install' ] && actions "$task"
          marked["$task"]=true
        else
          marked["$task"]=false
        fi
      fi
      ;;
  esac
}

function branch {
  case "$task" in
    $'fs:/usr/foo\nbar')
      "$operation" $'fs/layout:/usr/foo\nbar'
      "$operation" $'fs/ownership:/usr/foo\nbar'
      "$operation" $'fs/mode:/usr/foo\nbar'
      ;;
    $'fs/layout:/usr/foo\nbar')
      leaf
      ;;
    $'fs/ownership:/usr/foo\nbar')
      leaf
      ;;
    $'fs/mode:/usr/foo\nbar')
      leaf
      ;;
    $'pw/user:mo')
      leaf
      ;;
    *)
      return $ERR_UNKNOWN_TASK
      ;;
  esac
}


function dispatch {
  case "$1" in
    install|check|list|has)
      operation=$1
      ;;
    *)
      # Check if it's a number; that's how we'll communicate depth.
      if its_a_number
      then
        depth=$1
        shift
        dispatch "$@"
      else
        echo "Argument error." 1>&2
        exit $ERR_BAD_ARGS
      fi
      ;;
  esac
}

case "$2" in
  '')
    "$operation" $'fs:/usr/foo\nbar'
    "$operation" $'pw/user:mo'
    ;;
  *)
    "$operation" "$2"
    ;;
esac

