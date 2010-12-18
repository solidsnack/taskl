#!/bin/bash

find . -name '*.hs' -type f -print0 |
xargs -0 sed -nr '/^\{-# LANGUAGE /,/ #-\}$/ { s/^.+ ([^ ]+)$/\1/ ; p }' |
sort | uniq | fgrep -v '#-}'

