#!/bin/bash
pushd $1
if [ $? != 0 ]; then
  echo "Repository folder not found!"
  exit 1
fi
git clean -d -f -x
if [ $? != 0 ]; then
  echo "Could not clean repository"
  exit 1
fi
git pull
if [ $? != 0 ]; then
  echo "Could not call git pull"
  exit 1
fi
git reset --hard $2
if [ $? != 0 ]; then
  echo "Could not update to given commit hash"
  exit 1
fi
