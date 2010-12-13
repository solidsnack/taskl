#!/bin/bash

function m {
  echo "$@" 1>&2
}

function trial {
  m '(((' trial
  { m '(((' trial0
    "$@"
    m ')))' trial0 ;}
  set -e
  { m '(((' trial1
    "$@"
    m ')))' trial1 ;}
  m ')))' trial
}

m '(((' begin
trial "$@"
m ')))' end

