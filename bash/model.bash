#!/bin/bash
function usage {
name=`basename "$0"`
cat <<USAGE
 USAGE: $name <output spec>? (--wait|--no-wait)? <action>? <dest>? <tasks>+

  By default, all tasks are installed to the root. You may specify whether to
  install, check or merely list tasks; to use a different destination
  directory and to work with only a subset of tasks.

  Options and arguments may be given in any order, any number of times.

  To determine which action to perform, use one of these words:

    install  check  list

  To specify the installation directory, use a string matching the regex:

    (./|/).*

  Specifying tasks not in the script will cause it to fail immediately. To
  specify tasks, use strings of the form:

    (fs/.*|pw/.*|task):.*

  The script may be run with nulls between each output message, with newlines
  (the default) or with nulls and newlines.

    --nl  --0  -0  --0nl

  Options also control whether the script can be controlled by user (or
  machine) input. (The default is not to wait.)

    --no-wait  --wait

  When in \`wait' mode, the script will wait to receive OK or ABORT on the
  standard input after each statement. It will abort the installation if it
  receives the latter message.
USAGE
}




################################################################
# Script setup, handling of switches and arguments.

# Note that `-o' means enable while `+o' means disable.
set -o errexit
set -o nounset
set -o pipefail
set -o functrace
set -o errtrace
set -o noglob
set +o histexpand

function main {
  while "$1"
  do
    case "$1"
      --no-wait)                taskl_options[interactive]=false ;;
      --wait)                   taskl_options[interactive]=true ;;
      --nl|--0nl)               taskl_options[separator]=$1 ;;
      -0|--0)                   taskl_options[separator]='--0' ;;
      --help|-h|'-?')           usage ; exit 0 ;;
      install|check|list)       taskl_options[action]=$1 ;;
      ./*|/*)                   taskl_options[destination]="$1" ;;
      fs/*:*|pw/*:*|task:*)     { [ ${taskl_enabled["$1"]:-x} != x ] &&
                                  taskl_enabled["$1"]=true ;} ||
                                { ! echo "No such task \`$1'." 1>&2 ;} ;;
      *)                        ! echo 'Invalid arguments.' 1>&2 ;;
    esac
    shift
  done
}




################################################################
# Task\L machine -- functions and state.

declare -A taskl_checks taskl_enabled
declare -A taskl_options=(  [separator]=--nl  [destination]=/
                            [action]=install  [interactive]=false  )

function taskl_message {
  case ${taskl_options[separator]} in
    --0nl)          printf      " %s %s\\0\\n"    "$2" "$3" ;;
    --0)            printf      " %s %s\\0"       "$2" "$3" ;;
    --nl)           printf      " %s %s\\n"       "$2" "$3" ;;
    *)              printf      " %s %s\\n"       "$2" "$3" ;;
  esac
}
function taskl_interact {
  if ${taskl_options[interactive]}
  then
    read response
    case "$response" in
      OK|ok)        taskl_info  "Read \`OK', proceeding." ;;
      ABORT|abort)  taskl_abort "Read \`ABORT', quitting." ;;
      *)            taskl_error "Read \`$response'; not understood." ;;
    esac
  fi
}
function taskl_enter {
  ${taskl_enabled["$1"]} && taskl_message P '>>' "$1" && interact
}
function taskl_check {
  ${taskl_enabled["$1"]} && taskl_message G '**' "$1" && interact
}
function taskl_enable {
  ${taskl_enabled["$1"]} &&
  ${taskl_checks["$1"]} &&
  taskl_message P '++' "$1" &&
  interact
}
function taskl_exec {
  ${taskl_enabled["$1"]} &&
  ${taskl_checks["$1"]} &&
  taskl_message G '@@' "$1" &&
  { [ 'check' = ${taskl_options[action]} ] &&
    { taskl_info 'Not running: dry-run mode.' ; false ;} ;} || true
}
function taskl_leave {
  ${taskl_enabled["$1"]} && taskl_message P '<<' "$1" && interact
}
function taskl_info {
  taskl_message B '##' "$1"
}
function taskl_abort {
  taskl_message Y '~~' "$1"
  exit 0
}
function taskl_error {
  taskl_message R '!!' "$1"
  exit 1
}




################################################################
# Generated code.

script_random_key=c7fd4f07-8007-4c9f-a7aa-c0cf581cf97b

taskl_enabled=(
[$'task:pg']=false
[$'fs:/etc/postgres.conf']=false
)

taskl_checks=(
[$'task:pg']=true
[$'fs:/etc/postgres.conf']=false
)

function apply {
  taskl_enter $'task:pg'
  taskl_enable $'task:pg' &&
  { taskl_enabled[$'fs:/etc/postgres.conf']=true
    taskl_enabled[$'fs:/etc/hosts']=true ;}
  taskl_enter $'fs:/etc/postgres.conf'
  taskl_check $'fs:/etc/postgres.conf' &&
  { ! diff -q "$1"/./etc/postgres.conf "$2"/etc/postgres.conf &&
    taskl_checks[$'fs:/etc/postgres.conf']=true ;}
  taskl_exec $'fs:/etc/postgres.conf' &&
  { cp -a "$1"/./etc/postgres.conf "$2"/etc/postgres.conf ;}
  taskl_leave $'fs:/etc/postgres.conf'
  taskl_enter $'task:postfix'
  taskl_enable $'task:postfix' &&
  { taskl_enabled[$'fs:/etc/postgres.conf']=true
    taskl_enabled[$'fs:/etc/hosts']=true ;}
  taskl_enter $'fs:/etc/hosts'
  taskl_check $'fs:/etc/hosts' &&
  { ! diff -q "$1"/./etc/hosts "$2"/etc/hosts
    taskl_checks[$'fs:/etc/hosts']=true ;}
  taskl_exec $'fs:/etc/hosts' &&
  { cp -a "$1"/./etc/hosts "$2"/etc/hosts ;}
  taskl_leave $'fs:/etc/hosts'
  taskl_leave $'task:pg'
  taskl_enter $'fs:/etc/postfix.conf'
  taskl_check $'fs:/etc/postfix.conf' &&
  { check_state=false
    ! diff -q "$1"/./etc/postfix.conf "$2"/etc/postfix.conf
    check_state=true ;}
  taskl_exec $'fs:/etc/postfix.conf' &&
  { cp -a "$1"/./etc/postfix.conf "$1"/etc/postfix.conf ;}
  taskl_leave $'fs:/etc/postfix.conf'
  taskl_leave $'task:postfix'
}




################################################################
# Go.

if fgprep $script_random_key "$0"     # Don't run code if we're being sourced. 
then
  main "$@"
  local dir=`dirname "$0"`/root    # Only run code from the package directory.
  cd "$dir"
  local physical_dir=`pwd -P`
  apply "$physical_dir" "${taskl_options[destination]}"
fi

