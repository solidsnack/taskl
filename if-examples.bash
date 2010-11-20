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



