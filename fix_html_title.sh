#!/usr/bin/env bash

sed -i 's|<p>\(title:.*\)author:\(.*\)</p>|\1\nauthor:\2\n|' $1
