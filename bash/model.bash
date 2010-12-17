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

    (fs/.+|pw/.+|task):.+
    (fs|pw):.+

  The latter form is a shortcut.

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

# Enable error handling and reporting.
set -o errexit -o errtrace -o nounset -o pipefail -o functrace
# Disable cute expansions that should not be available anyways.
set +o histexpand -o noglob
# Ignore environment variables that set options. (Also affects user IDs.)
set -o privileged

function main {
  local -a tasks_to_enable
  while [ "${1:-}" != '' ]
  do
    case "$1" in
      --no-wait)                taskl_options[interactive]=false ;;
      --wait)                   taskl_options[interactive]=true ;;
      --nl|--0nl)               taskl_options[separator]=$1 ;;
      -0|--0)                   taskl_options[separator]='--0' ;;
      --help|-h|'-?')           usage ; exit 0 ;;
      install|check|list)       taskl_options[action]=$1 ;;
      ./*|/*)                   taskl_options[destination]="$1" ;;
      fs/*:*|pw/*:*|task:*)     if [ ${taskl_enabled["$1"]:-x} != x ]
                                then
                                  i=${#tasks_to_enable[@]}
                                  tasks_to_enable[$i]="$1"
                                else
                                  ! echo "No such task \`$1'." 1>&2
                                fi ;;
      fs:*)                     base=${1#fs:}
                                begin=${#tasks_to_enable[@]}
                                for t in fs/{own,node,mode}:"$base"
                                do
                                  if [ ${taskl_enabled["$t"]:-x} != x ]
                                  then
                                    i=${#tasks_to_enable[@]}
                                    tasks_to_enable[$i]="$t"
                                  fi
                                done
                                if [ $begin = ${#tasks_to_enable[@]} ]
                                then
                                  ! echo "No such task \`$1'." 1>&2
                                fi ;;
      pw:*)                     base=${1#pw:}
                                begin=${#tasks_to_enable[@]}
                                for t in pw/{user,group,members}:"$base"
                                do
                                  if [ ${taskl_enabled["$t"]:-x} != x ]
                                  then
                                    i=${#tasks_to_enable[@]}
                                    tasks_to_enable[$i]="$t"
                                  fi
                                done
                                if [ $begin = ${#tasks_to_enable[@]} ]
                                then
                                  ! echo "No such task \`$1'." 1>&2
                                fi ;;
      *)                        ! echo 'Invalid arguments.' 1>&2 ;;
    esac
    shift
  done
  if [ ${#tasks_to_enable[@]} != 0 ]
  then
    for task in "${!taskl_enabled[@]}"
    do
      taskl_enabled["$task"]=false
    done
    for i in "${!tasks_to_enable[@]}"
    do
      taskl_enabled["${tasks_to_enable[$i]}"]=true
    done
  fi
}




################################################################
# Implementation of IdemShell commands and tests.

function idem_CHOWN {
  local user="${2%:*}"
  local group="${2#*:}"
  [ ! -z "$user" ]  && chown "$user"  "$1"
  [ ! -z "$group" ] && chgrp "$group" "$1"
}
function idem_CHMOD {
  chmod "$2" "$1"
}
function idem_RM {
  rm -rf "$1"
}
function idem_CP {
  { which rsync && rsync "$1" "$2" ;} || cp "$1" "$2"
}
function idem_LNs {
  { [ -L "$2" ] && rm -f "$2" ;} || rm -rf "$2"
  ln -s "$1" "$2"
}
function idem_TOUCH {
  rm -rf "$1"
  touch "$1"
}
function idem_MKDIR {
  rm -rf "$1"
  mkdir -p "$1"
}
function idem_USERADD {
  idem_USERDEL "$1"
  useradd "$@" 
}
function idem_USERDEL {
  getent passwd "$1" && userdel "$1"
}
function idem_GROUPADD {
  idem_GROUPDEL "$1"
  groupadd "$@" 
}
function idem_GROUPDEL {
  getent group "$1" && groupdel "$1"
}
function idem_GPASSWDa {
  gpasswd "$1" -a "$2" 1>/dev/null
}
function idem_GPASSWDd {
  gpasswd "$1" -d "$2" 1>/dev/null
}

function idem_LSo {
  local user="${2%:*}"
  local group="${2#*:}"
  { [ "$user" = "" ]  || idem_helper_LSo "$1" ":$user"  ;} &&
  { [ "$group" = "" ] || idem_helper_LSo "$1" "$group:" ;}
}
function idem_helper_LSo {
  local awk_script
  local name
  case "$2" in
    *:) name="${2%:}" ; awk_script='{print $3}' ;;
    :*) name="${2#:}" ; awk_script='{print $4}' ;;
    *)  ! echo 'Mysterious invalid call to LSo helper.' 1>&2 ;;  
  esac
  local normed="${name#+}"
  if [ "$name" = "$normed" ]  # Determine if we are using numric form.
  then
    ls -ld "$1"
  else
    ls -nd "$1"
  fi | awk "$awk_script" | fgrep -x -- "$normed"
}
function idem_LSm {
  ls -ld "$1" | awk '{print $1}' | egrep ".$2"
}
function idem_DASHe {
  [ -e "$1" ]
}
function idem_DASH_ {
  [ "$1" "$2" ]
}
function idem_DIFFq {
  diff -q "$1" "$2" 2>/dev/null
}
function idem_LSl {
  [ `readlink -- "$2"` = "$1" ]
}
function idem_GETENTu {
  getent passwd "$1"
}
function idem_GETENTg {
  getent group "$1"
}
function idem_GROUPS {
  groups -- "$1" | xargs printf '%s\n' | sed '1,2 d' | fgrep -x -- "$2"
}
function idem_Not {
  ! echo 'Unimplemented IdemShell primitive should not be called!' 1>&2
}
function idem_And {
  ! echo 'Unimplemented IdemShell primitive should not be called!' 1>&2
}
function idem_Or {
  ! echo 'Unimplemented IdemShell primitive should not be called!' 1>&2
}
function idem_TRUE {
  ! echo 'Unimplemented IdemShell primitive should not be called!' 1>&2
}
function idem_FALSE {
  ! echo 'Unimplemented IdemShell primitive should not be called!' 1>&2
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
  if ${taskl_enabled["$1"]} && [ 'list' != ${taskl_options[action]} ]
  then
    taskl_message P '>>' "$1"
    taskl_interact
  fi
}
function taskl_check {
  ${taskl_enabled["$1"]} &&
  [ 'list' != ${taskl_options[action]} ] &&
  taskl_message G '**' "$1" &&
  taskl_interact
}
function taskl_enable {
  ${taskl_enabled["$1"]} &&
  ! ${taskl_checks["$1"]} &&
  { [ 'list' = ${taskl_options[action]} ] || taskl_message P '++' "$1" ;} &&
  taskl_interact
}
function taskl_exec {
  ${taskl_enabled["$1"]} &&
  [ 'list' != ${taskl_options[action]} ] &&
  ! ${taskl_checks["$1"]} &&
  { taskl_message G '@@' "$1"
    if [ 'check' = ${taskl_options[action]} ]
    then
      taskl_info 'Not running: dry-run mode.'
      false
    fi ;}
}
function taskl_leave {
  if ${taskl_enabled["$1"]}
  then
    taskl_message P '<<' "$1"
    taskl_interact
  fi
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

# State arrays.
taskl_enabled=(
[$'fs/node:/q']=false
[$'fs/node:/q/a']=true
[$'fs/node:/q/b']=true
[$'fs/node:/q/p']=false
)
taskl_checks=(
[$'fs/node:/q']=false
[$'fs/node:/q/a']=false
[$'fs/node:/q/b']=false
[$'fs/node:/q/p']=false
)

# Installation routine.
function apply {
  taskl_enter $'fs/node:/q/a'
  taskl_enter $'fs/node:/q/b'
  if taskl_check $'fs/node:/q/a'
  then
    if idem_DASH_ -f /q/a
    then
      taskl_checks[$'fs/node:/q/a']=true
    fi
  fi
  if taskl_check $'fs/node:/q/b'
  then
    if idem_DASH_ -f /q/b
    then
      taskl_checks[$'fs/node:/q/b']=true
    fi
  fi
  if taskl_enable $'fs/node:/q/a'
  then
    for task in $'fs/node:/q'
    do
      taskl_enabled["$task"]=true
    done
  fi
  if taskl_enable $'fs/node:/q/b'
  then
    for task in $'fs/node:/q' $'fs/node:/q/p'
    do
      taskl_enabled["$task"]=true
    done
  fi
  taskl_enter $'fs/node:/q'
  if taskl_check $'fs/node:/q'
  then
    if idem_DASH_ -d /q
    then
      taskl_checks[$'fs/node:/q']=true
    fi
  fi
  if taskl_exec $'fs/node:/q'
  then
    idem_MKDIR /q
  fi
  taskl_leave $'fs/node:/q'
  if taskl_exec $'fs/node:/q/a'
  then
    idem_TOUCH /q/a
  fi
  taskl_leave $'fs/node:/q/a'
  taskl_enter $'fs/node:/q/p'
  if taskl_check $'fs/node:/q/p'
  then
    if idem_DASH_ -d /q/p
    then
      taskl_checks[$'fs/node:/q/p']=true
    fi
  fi
  if taskl_exec $'fs/node:/q/p'
  then
    idem_MKDIR /q/p
  fi
  taskl_leave $'fs/node:/q/p'
  if taskl_exec $'fs/node:/q/b'
  then
    idem_TOUCH /q/b
  fi
  taskl_leave $'fs/node:/q/b'
}




################################################################
# Go.

if fgrep -q $script_random_key "$0"   # Don't run code if we're being sourced.
then
  main "$@"
  dir=`dirname "$0"`/root          # Only run code from the package directory.
  cd "$dir"
  physical_dir=`pwd -P`
  apply "$physical_dir" "${taskl_options[destination]}"
fi

