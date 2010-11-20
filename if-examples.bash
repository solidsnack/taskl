#!/bin/bash

# All take the 'then' branch.

if false; true
then
  echo ok
else
  echo failed
fi

if false
   true
then
  echo ok
else
  echo failed
fi

if { false
     true ;}
then
  echo ok
else
  echo failed
fi

if date \
     +%FT%TZ > /dev/null
   diff -q \
     $0 $0
then
  echo ok
else
  echo failed
fi


