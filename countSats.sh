#!/bin/bash
# countSats.sh
myvar="Sat"
i=$(grep $myvar ${1}* | wc -l)
echo "The number of files reported on a Saturday is $i"
