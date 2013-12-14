#!/bin/bash

set -ex

LINE_LIMIT=500
CURRENT_PATH="$( cd "$( dirname "$0" )" && pwd )"
NUMBER_OF_LINES=`find $CURRENT_PATH/../lib -name '*.rb' | xargs wc -l | tail -n1 | awk -F ' ' '{print $1}'`

test "$NUMBER_OF_LINES" -le "$LINE_LIMIT"
