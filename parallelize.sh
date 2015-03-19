#!/bin/bash


ips=$(cat "/etc/hosts" | awk '{print $1}' | grep "[0-9].[0-9].[0.9]")
login="foo"

for i in $ips
do
    addr=$login@"${i}"
    echo "${addr}"
    ssh addr './pi 1000000 >> results'
done
