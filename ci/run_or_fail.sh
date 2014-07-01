# helper method for providing error messages for a command
run_or_fail() {
  EXPLANATION=$1
  shift 1
  $@
  if [ $? != 0 ]; then
    echo $EXPLANATION
    exit 1
  fi
}
