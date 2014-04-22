#! /bin/bash

cd fleet

to_count=""
for f in *.py; do
    case $f in
        client.py) ;;
        member_single.py) ;;
        deterministic_network.py) ;;
        run.py) ;;
        *)  to_count="$f $to_count" ;;
    esac
done

wc -l $to_count
