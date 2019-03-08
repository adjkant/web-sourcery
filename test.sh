#!/bin/bash

if [ "$1" = "--top-only" ]
then
    raco test ./examples
else
    raco test ./web-sourcery/ ./examples
fi
