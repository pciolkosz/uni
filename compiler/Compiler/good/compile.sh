#!/bin/sh

cd ../
for file in good/*.lat; do
    ./latc_x86 $file
done
