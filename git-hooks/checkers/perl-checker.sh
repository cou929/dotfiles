#! /bin/sh

if [ ${#1} == 0 ]; then
    echo "Invalid argument" 1>&2
    exit 1
fi

perl -wc $1
exit $?