#!/bin/sh

count=0
output=""
while read line; do
	output="$output $line"
	count=`expr $count + 1`
	echo "$count" >&2
done
echo $output
sleep 1
exit 3
