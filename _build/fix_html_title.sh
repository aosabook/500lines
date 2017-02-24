#!/usr/bin/env bash

sed -i 's|<p>\(title:.*\)author:\(.*\)</p>|\1\
author:\2\
|' $1
