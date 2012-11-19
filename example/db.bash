#!/bin/bash
set -o errexit -o nounset -o pipefail

function tasks {
  aptitude -y install python-software-properties
  add-apt-repository $'ppa:pitti/postgresql'
  add-apt-repository $'ppa:nginx/stable'
  add-apt-repository $'ppa:chris-lea/node.js'
  aptitude update
  aptitude -y install postgresql-9.2 postgresql-9.2-dbg postgresql-contrib-9.2 \
           postgresql-client-9.2 postgresql-doc-9.2 postgresql-server-dev-9.2
  msg -_- //pg.root
  aptitude -y install postgresql-client-9.2
  msg -_- //pg.branch
  msg -_- //db
}

function curl_sh {
  local url="$1" ; shift
  curl_ "$url" | sh -s -- "$@"
}

function curl_ {
  curl -sSfL --retry 4 "$1"
}

function msg { out "$*" 1>&2 ;}
function err { local x=$? ; msg "$*" ; return $(( $x == 0 ? 1 : $x )) ;}
function out { printf '%s\n' "$*" ;}

if declare -F | cut -d' ' -f3 | fgrep -qx -- "${1:-}"
then "$@"       # Recognized function so call with arguments.
else tasks "$@" # Unrecognized so use default function.
fi


