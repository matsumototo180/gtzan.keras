#!/bin/sh

for file in $(ls $1)
do
  echo "$1$file/"
  sh convMP3toWAV.sh "$1$file/"
done
