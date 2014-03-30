#!/bin/bash
if [ -e ".commit_hash" ]; then
  rm .commit_hash
fi
pushd $1
if [ $? != 0 ]; then
  echo "Repository folder not found!"
  exit 1
fi
git reset --hard HEAD
if [ $? != 0 ]; then
  echo "Could not reset git"
  exit 1
fi
COMMIT=`git log -n1`
if [ $? != 0 ]; then
  echo "Could call 'git log' on repository"
  exit 1
fi
HASH=`echo $COMMIT | awk '{ print $2 }'`
git pull
if [ $? != 0 ]; then
  echo "Could not pull from repository"
  exit 1
fi
COMMIT=`git log -n1`
if [ $? != 0 ]; then
  echo "Could call 'git log' on repository"
  exit 1
fi
NEWHASH=`echo $COMMIT | awk '{ print $2 }'`
if [ $NEWHASH != $HASH ]; then
  popd
  echo $NEWHASH > .commit_hash
fi
