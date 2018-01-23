#!/bin/sh

for file in *.lat; do
    echo $file
    ./${file%.lat} >out
    diff out ${file%.lat}.output
done
