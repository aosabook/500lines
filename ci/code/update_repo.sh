#!/bin/bash

source run_or_fail.sh

# delete previous id 
rm -f .commit_id

# go to repo and update it to given commit
run_or_fail "Repository folder not found!" pushd $1 1> /dev/null
run_or_fail "Could not reset git" git reset --hard HEAD

# get the most recent commit
COMMIT=$(run_or_fail "Could not call 'git log' on repository" git log -n1)
if [ $? != 0 ]; then
  echo "Could not call 'git log' on repository"
  exit 1
fi
# get its id
COMMIT_ID=`echo $COMMIT | awk '{ print $2 }'`

# update the repo
run_or_fail "Could not pull from repository" git pull

# get the most recent commit
COMMIT=$(run_or_fail "Could not call 'git log' on repository" git log -n1)
if [ $? != 0 ]; then
  echo "Could not call 'git log' on repository"
  exit 1
fi
# get its id
NEW_COMMIT_ID=`echo $COMMIT | awk '{ print $2 }'`

# if the id changed, then write it to a file
if [ $NEW_COMMIT_ID != $COMMIT_ID ]; then
  popd 1> /dev/null
  echo $NEW_COMMIT_ID > .commit_id
fi
