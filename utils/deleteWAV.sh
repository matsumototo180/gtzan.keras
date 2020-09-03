#!/bin/sh

for file in $1*.wav
do
  rm -f $file
done