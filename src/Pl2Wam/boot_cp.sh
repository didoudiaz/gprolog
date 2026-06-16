#!/bin/sh

files=${*:-*.wam}
for i in $files; do
    \cp $i ${i}1
done


