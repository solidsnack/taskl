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


##  State storage.

check_state=false

declare -A options=(
[0]=false
[interactive]=false
)

declare -A enabled=(
[$'task:pg_conf']=false
[$'task:my_conf']=false
)


##  Notification, interaction and formatting utilities.

function message {
  if ${options[0]}
  then
    printf " %s %s\\0" "$2" "$3"
  else
    printf " %s %s\\n" "$2" "$3"
  fi
}

function interact {
  if ${options[interactive]}
  then
    read response
    case "$response" in
      OK)           info_notify  "Read \`OK', proceeding." ;;
      ABORT)        abort_notify "Read \`ABORT', quitting." ;;
      *)            error_notify "Read \`$response'; not understood." ;;
    esac
  fi
}

function check_notify {  ${enabled["$1"]} && message G '**' "$1" && interact  }
function exec_notify  {  ${enabled["$1"]} && message G '@@' "$1" && interact  }
function enter_notify {  ${enabled["$1"]} && message P '>>' "$1" && interact  }
function leave_notify {  ${enabled["$1"]} && message P '<<' "$1" && interact  }
function info_notify  {                      message B '##' "$1"              }
function abort_notify {                      message Y '~~' "$1" ; exit 0     }
function error_notify {                      message R '!!' "$1" ; exit 1     }




##  Handle input.

T=$1
for task in "${enabled[@]}"
do
  enabled["$task"]=true
done




##  Installation code.

enter_notify $'task:pg'
check_state=true ## No check.
{ ${enabled[$'task:pg']}
  ${check_state}
  enabled[$'fs:/etc/postgres.conf']=true
  enabled[$'fs:/etc/hosts']=true }
enter_notify $'fs:/etc/postgres.conf'
check_notify $'fs:/etc/postgres.conf'
{ ${enabled[$'fs:/etc/postgres.conf']}
  check_state=false
  ! diff -q ./etc/postgres.conf "$T"/etc/postgres.conf
  check_state=true }
exec_notify $'fs:/etc/postgres.conf'
{ ${enabled[$'fs:/etc/postgres.conf']}
  ${check_state}
  exec_notify $'fs:/etc/postgres.conf'
  cp -a ./etc/postgres.conf "$T"/etc/postgres.conf }
leave_notify $'fs:/etc/postgres.conf'
enter_notify $'task:postfix'
check_state=true ## No check.
{ ${enabled[$'task:postfix']}
  ${check_state}
  enabled[$'fs:/etc/postgres.conf']=true
  enabled[$'fs:/etc/hosts']=true }
enter_notify $'fs:/etc/hosts'
check_notify $'fs:/etc/hosts'
{ ${enabled[$'fs:/etc/hosts']}
  check_state=false
  ! diff -q ./etc/hosts "$T"/etc/hosts
  check_state=true }
exec_notify $'fs:/etc/hosts'
{ ${enabled[$'fs:/etc/hosts']}
  ${check_state}
  exec_notify $'fs:/etc/hosts'
  cp -a ./etc/hosts "$T"/etc/hosts }
leave_notify $'fs:/etc/hosts'
leave_notify $'task:pg'
enter_notify $'fs:/etc/postfix.conf'
check_notify $'fs:/etc/postfix.conf'
{ ${enabled[$'fs:/etc/postfix.conf']}
  check_state=false
  ! diff -q ./etc/postfix.conf "$T"/etc/postfix.conf
  check_state=true }
exec_notify $'fs:/etc/postfix.conf'
{ ${enabled[$'fs:/etc/postfix.conf']}
  ${check_state}
  exec_notify $'fs:/etc/postfix.conf'
  cp -a ./etc/postfix.conf "$T"/etc/postfix.conf }
leave_notify $'fs:/etc/postfix.conf'
leave_notify $'task:postfix'

