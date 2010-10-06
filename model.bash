#!/bin/bash

# Note that `-o' means enable while `+o' means disable.
set -o errexit
set -o nounset
set -o pipefail
set -o functrace
set -o errtrace
set -o noglob
set +o histexpand

spaces='                                        '

declare -A schedule
notified[$'fs:/usr/foo\nbar']=false
notified[$'fs/obj:/usr/foo\nbar']=false
notified[$'fs/own:/usr/foo\nbar']=false
notified[$'fs/mode:/usr/foo\nbar']=false
notified[$'user:mo']=false

declare -A notified
notified[$'fs:/usr/foo\nbar']=false
notified[$'fs/obj:/usr/foo\nbar']=false
notified[$'fs/own:/usr/foo\nbar']=false
notified[$'fs/mode:/usr/foo\nbar']=false
notified[$'user:mo']=false

declare -A entered
entered[$'fs:/usr/foo\nbar']=false
entered[$'fs/obj:/usr/foo\nbar']=false
entered[$'fs/own:/usr/foo\nbar']=false
entered[$'fs/mode:/usr/foo\nbar']=false
entered[$'user:mo']=false

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
  esac
}

# Place items in the schedule as needed. Only composite tasks get their own
# case statements; atomic tasks default to check and schedule.
declare -a stack=()
function mark {
  local task="${1}"
  stack[${#tasks_stack[@]}]="$task"
  case "$task" in
    $'fs:/usr/foo\nbar')
      mark $'fs/obj:/usr/foo\nbar'
      mark $'fs/own:/usr/foo\nbar'
      mark $'fs/mode:/usr/foo\nbar'
      ;;
    *)
      if check ${task}
      then
        for i in ${!stack[@]}
        do
          local this_task=stack[$i]
          if ! schedule["${this_task}"]
          then
            for x in `seq 1 $i`
            do
              echo -n ' '
            done
            echo -n '+ '
            echo "${i}" 
            schedule[$i]=true
          fi
        done
      fi
      ;;
  esac
  unset stack[${#tasks_stack[@]}-1]
}


