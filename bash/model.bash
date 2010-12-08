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

# Hash for run modes.
declare -A options=(
[sep]=--nl
[dest]=/
[wait]=false
[action]=install
)

while "$1"
do
  case "$1"
    --no-wait)                  options[wait]=false ;;
    --wait)                     options[wait]=true ;;
    --nl|--0nl)                 options[sep]=$1 ;;
    -0|--0)                     options[sep]='--0' ;;
    --help|-h|'-?')             usage ; exit 0 ;;
    install|check|list)         options[action]=$1 ;;
    ./*|/*)                     options[dest]="$1" ;;
    fs/*:*|pw/*:*|task:*)       { [ ${enabled["$1"]:-x} != x ] &&
                                  enabled["$1"]=true ;} ||
                                  { ! echo "No such task \`$1'." 1>&2 ;}
                                fi ;;
    *)                          ! echo 'Invalid arguments.' 1>&2 ;;
  esac
  shift
done




################################################################
# Task\L library functions.

function taskl_message {
  case $sep in
    --0nl)          printf      " %s %s\\0\\n"    "$2" "$3" ;;
    --0)            printf      " %s %s\\0"       "$2" "$3" ;;
    --nl)           printf      " %s %s\\n"       "$2" "$3" ;;
    *)              printf      " %s %s\\n"       "$2" "$3" ;;
  esac
}
function taskl_interact {
  if ${options[interactive]}
  then
    read response
    case "$response" in
      OK)           taskl_info  "Read \`OK', proceeding." ;;
      ABORT)        taskl_abort "Read \`ABORT', quitting." ;;
      *)            taskl_error "Read \`$response'; not understood." ;;
    esac
  fi
}
function taskl_enter {
  ${enabled["$1"]} && taskl_message P '>>' "$1" && interact
}
function taskl_check {
  ${enabled["$1"]} && taskl_message G '**' "$1" && interact
}
function taskl_enable {
  ${enabled["$1"]} && taskl_message P '++' "$1" && interact
}
function taskl_exec {
  ${enabled["$1"]} && check_state && taskl_message G '@@' "$1" &&
    { [ 'check' = ${options[action]} ] &&
      { taskl_info 'Not running: dry-run mode.' 
        false                                   ;} ;} || true
}
function taskl_leave {
  ${enabled["$1"]} && taskl_message P '<<' "$1" && interact
}
function taskl_info {
  taskl_message B '##' "$1"
}
function taskl_abort {
  taskl_message Y '~~' "$1" ; exit 0
}
function taskl_error {
  taskl_message R '!!' "$1" ; exit 1
}




################################################################
# Task\L state variables.

check_state=false

declare -A enabled




################################################################
# Generated Code

enabled=(
[$'task:pg_conf']=false
[$'task:my_conf']=false
)

msg_enter $'task:pg'
check_state=true ## No check.
{ ${enabled[$'task:pg']}
  ${check_state}
  enabled[$'fs:/etc/postgres.conf']=true
  enabled[$'fs:/etc/hosts']=true ;}
msg_enter $'fs:/etc/postgres.conf'
msg_check $'fs:/etc/postgres.conf' &&
{ check_state=false
  ! diff -q ./etc/postgres.conf "$T"/etc/postgres.conf
  check_state=true ;}
msg_exec $'fs:/etc/postgres.conf' &&
{ cp -a ./etc/postgres.conf "$T"/etc/postgres.conf ;}
msg_leave $'fs:/etc/postgres.conf'
msg_enter $'task:postfix'
check_state=true ## No check.
{ ${enabled[$'task:postfix']}
  ${check_state}
  enabled[$'fs:/etc/postgres.conf']=true
  enabled[$'fs:/etc/hosts']=true ;}
msg_enter $'fs:/etc/hosts'
msg_check $'fs:/etc/hosts' &&
{ check_state=false
  ! diff -q ./etc/hosts "$T"/etc/hosts
  check_state=true ;}
msg_exec $'fs:/etc/hosts'
{ cp -a ./etc/hosts "$T"/etc/hosts ;}
msg_leave $'fs:/etc/hosts'
msg_leave $'task:pg'
msg_enter $'fs:/etc/postfix.conf'
msg_check $'fs:/etc/postfix.conf' &&
{ check_state=false
  ! diff -q ./etc/postfix.conf "$T"/etc/postfix.conf
  check_state=true ;}
msg_exec $'fs:/etc/postfix.conf' &&
{ cp -a ./etc/postfix.conf "$T"/etc/postfix.conf ;}
msg_leave $'fs:/etc/postfix.conf'
msg_leave $'task:postfix'

