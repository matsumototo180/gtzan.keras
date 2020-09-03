#!/bin/sh

for file in $1*.mp3
do
	name="$1$(basename $file .mp3).wav"
	ffmpeg -i $file -ar 22050 -ac 1 $name
done

for file in $1*.mp3
do
	rm -f $file
done
