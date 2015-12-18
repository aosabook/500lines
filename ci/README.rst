SuperSimpleCI
=============
**Author:** Malini Das

**Project:** SuperSimpleCI

**Requirements:** Posix, Python2.7+, Git

This is a simplified version of a distributed continuous integration (CI) system. 
It assumes you're running against a git repository, and needs git on the PATH.

Files
=====
* repo_observer.py -- Checks the repo for changes and notifies the dispatcher
* dispatcher.py -- Receives test requests and dispatches them against test runners
* test_runner.py -- Runs the tests and returns the results 
* helpers.py -- Holds shared code
* update_repo.sh -- Updates the shared repo and drops a new file with the commit id if there's a change
* test_runner_script.sh -- Updates the test runner's repository to the given commit id 
* run_or_fail.sh -- Helper method used in update_repo.sh and test_runner_script.sh
* tests/ -- Holds some demo tests to run

Idea
====

At its core, a continuous integration system contains 3 parts: an observer,
a dispatcher and at least one test runner. The observer checks the main
repository for changes, and upon a change, will notify the dispatcher. The
dispatcher will then be responsible for dispatching a test runner against that
particular commit. There can be many test runners, which is useful when you have
many tests, and when tests take very long. The dispatcher is in charge of
maintaining a list of usable test runners.

In this project, we have one observer, one dispatcher, and as many test
runners as you'd like. We start the dispatcher first, then a test runner. The
test runner will register itself with the dispatcher to let it know its
host/port and that it is available. After that, we start the observer, and
we can start changing our master repository to see the system in action.

Running the CI
==============

File Setup
----------
To get the example working, we need to have a repository for this CI system to
check against. Let's start a local git repository in a separate folder.
Let's call this test_repo::

  mkdir test_repo
  git init

This will be our master repository. This is where developers check in their code,
and so our CI should pull this repository and check for commits, then run
tests. The thing that checks for new commits is the repo_observer.py file.

The repo observer works by checking commits, so we need at least one commit in
the repo. Let’s commit our example tests so we have some tests to run.

NOTE: due to the limitations of code length and unittest, I simplified test
discovery. We will *only* run tests that are in a directory named "tests" within
the repository. So let's set this up.

Copy the tests/ folder from this code base to test_repo and commit it::

  cp -r /this/directory/tests /path/to/test_repo/
  cd /path/to/test_repo
  git add tests/
  git commit -m "add tests"

The repo observer will need its own clone of the code::

  git clone /path/to/test_repo test_repo_clone_obs

this will be the repo used by the repo observer. It will check this repo for
changes from the master.

The test runner will *also* need its own clone of the code::

  git clone /path/to/test_repo test_repo_clone_runner

The test runner will use this clone to checkout the commit it needs to test.

Running the Code
----------------

The repo observer merely checks for changes. It does not do any test running.
Instead, it notifies a main server in dispatcher.py of a change. Dispatcher.py
will handle any incoming test request changes by dispatching the request to
a testrunner.

For our CI system, let's start the dispatcher first, running on port 8888::

  python dispatcher.py

In a new shell, we should start the test_runner (so it can register itself with the
dispatcher)::

  python test_runner.py <path/to/test_repo_clone_runner>

The test runner will assign itself its own port, in the range 8900->9000. You
may run as many test runners as you like.

Lastly, in another new shell, let's start the repo_observer::

  python repo_observer.py --dispatcher-server=localhost:8888 <path/to/test_repo_clone_obs>

Now that everything is set up, let's trigger some tests! To do that, we'll need
to make a new commit. Go to your master repo and make an arbitrary change::

  cd /path/to/test_repo
  touch new_file
  git add new_file
  git commit -m "new file" new_file

then repo_observer.py will realize that there's a new commit and will notify
the dispatcher. You can see the output in their respective shells, so you
can monitor them. Once the dispatcher receives the test results, it stores them
in a test_results/ folder in this code base, using the commit id as the
file name.

Error Handling
==============

If you kill the test_runner.py process, dispatcher.py will figure out that
the runner is no longer available and will remove it from the pool.

You can also kill the test runner before it runs a test, to simulate a 
machine going down. If you do so, the dispatcher will realize the 
runner went down and will wait for a new runner to connect. Upon 
connection, it will dispatch the unfinished commit to it.

If you kill the dispatcher, the repo observer will figure out it went down
and will throw an exception. The test runners will also notice, and will
shut down.
