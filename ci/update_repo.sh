#!/bin/bash

source run_or_fail.sh

# delete previous hash
rm -f .commit_hash

# go to repo and update it to given commit
run_or_fail "Repository folder not found!" pushd $1
run_or_fail "Could not reset git" git reset --hard HEAD

# get the most recent commit
COMMIT=`git log -n1`
if [ $? != 0 ]; then
  echo "Could not call 'git log' on repository"
  exit 1
fi
# get its hash
HASH=`echo $COMMIT | awk '{ print $2 }'`

# update the repo
run_or_fail "Could not pull from repository" git pull

# get the most recent commit
COMMIT=`git log -n1`
if [ $? != 0 ]; then
  echo "Could not call 'git log' on repository"
  exit 1
fi
# get its hash
NEWHASH=`echo $COMMIT | awk '{ print $2 }'`

# if the hash changed, then write it to a file
if [ $NEWHASH != $HASH ]; then
  popd
  echo $NEWHASH > .commit_hash
fi
