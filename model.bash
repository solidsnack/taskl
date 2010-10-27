#!/bin/bash
function usage {
name=`basename "$0"`
cat <<USAGE
 USAGE: $name (--0|--nl|--0nl)? (--wait|--no-wait)? <action>? <dest>? <tasks>+

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

    (fs|pw|task):.*

  The script may be run with nulls between each output message, with newlines
  (the default) or with nulls and newlines.

    --nl  --0  --0nl

  Options also control whether the script can be controlled by user (or
  machine) input. (The default is not to wait.)

    --no-wait  --wait

  When in \`wait' mode, the script will wait to receive OK or ABORT on the
  standard input after each statement. It will abort the installation if it
  receives the latter message.
USAGE
}



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
[sep]=--nl
[dest]=/
[wait]=false
[action]=install
)

declare -A enabled=(
[$'task:pg_conf']=false
[$'task:my_conf']=false
)


##  Notification, interaction and formatting utilities.

function message {
  case $sep in
    --0nl)          printf      " %s %s\\0\\n"    "$2" "$3" ;;
    --0)            printf      " %s %s\\0"       "$2" "$3" ;;
    --nl)           printf      " %s %s\\n"       "$2" "$3" ;;
    *)              printf      " %s %s\\n"       "$2" "$3" ;;
  esac
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

while "$1"
do
  case "$1"
    --no-wait)                  options[wait]=false ;;
    --wait)                     options[wait]=true ;;
    --0|--nl|--0nl)             options[sep]=$1 ;;
    --help|-h|'-?')             usage ; exit 0 ;;
    install|check|list)         options[action]=$1 ;;
    ./*|/*)                     options[dest]="$1" ;;
    fs:*|task:*|pw:*)           if [ ${enabled["$1"]:-x} != x ]
                                then
                                  enabled["$1"]=true
                                else
                                  error_notify "No such task \`$1'."
                                fi ;;
    *)                          error_notify 'Invalid arguments.' ;;
  esac
  shift
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

