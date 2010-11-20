#!/bin/bash

# All take the 'then' branch.

if false; true
then
  echo then
else
  echo else
fi

if false
   true
then
  echo then
else
  echo else
fi

if { false
     true ;}
then
  echo then
else
  echo else
fi



