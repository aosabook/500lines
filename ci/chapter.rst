What is a Continuous Integration System?
========================================

When developing software, we want to be able to verify that our new features or bugfixes are safe and work as expected. We do this by running tests against our code. Sometimes, developers will run tests locally to verify that their changes are safe, but developer smay not have the time to test their code on every system their software runs in, and when more and more tests get added, the amount of time to run them locally increases, and it becomes increasingly inefficient. Because of this, continuous integration systems have been created.

Continuous Integration (CI) systems are dedicated systems used to test new code. Upon a commit to the code repository, it is the responsibility of the continuous integration system to verify that this commit will not break any tests. To do this, the system must be able to fetch the new changes, run the tests and report its results. Like any other system, it should also be failure resistant. This means if any part of the system fails, it should be able to recover and continue from that point. This test system should also handle load well, so that we can get test results in a reasonable about of time in the event that commits are being made faster than the tests can be run. We can achieve this by distributing and parallelizing the testing effort. This project will demonstrate a small-scale, distributed continuous integration system. It is a bare-bones system, but is designed for extensibility.


Project Limitations
===================

This project uses Git as the code repository that needs to be tested. Only standard source code management calls will be used, so if you are unfamiliar with Git but are familiar with other version control systems like svn or Mercurial, you can still follow along.

This project also assumes you are using a POSIX system.

Due to the limitations of code length and unittest, I simplified test
discovery. We will *only* run tests that are in a directory named "tests" within
the repository.

Continusous integration systems monitor a master repository which is usually hosted on a webserver, and not local to the CI's filesystems. For the cases of our example, we will use a local repository instead of a remote repository.

Continuous integration systems need not run periodically. You can also have them run every few commits, or per-commit. For our example case, I am simplifying this to just being periodically. This means that if this is set up to check for changes in 5 minute periods, it will run tests against the most recent commit made after the 5 minute period. It won't test every commit made within that period of time, only the most recent one.

This CI system is designed to check periodically for changes in a repository. In real-world CI systems, you can also have the repository observer get notified by a hosted repository. Github, for example, provides 'post-commit hooks' which send out notifications to a URL. Following this model, the repository observer would be called by the webserver hosted at that URL to respond to that notification. Since this is complex to model locally, we're using an observer model, where the repository observer will check for changes instead of being notified.

CI systems also have a reporter aspect to them, where the test runner reports its results to a reporter component that gathers results and makes them available for people to see like on a webpage. For simplicity, this project gathers the test results and stores them as files in the filesystem local to the dispatcher process.

Introduction
============

This basic continuous integration (CI) system has 3 components: a listener or watcher for changes in the repository, a test job dispatcher, and a test runner. The first component is used to observe the repository. When it notices that a commit has been made, then it must notify the job dispatcher. The job dispatcher then finds a test runner and gives it the commit number to test.  In this project, each of these components is its own process, and can be run on separate machines if you wish. In real world systems, they are run in a distributed environment so we can have failover redundancy (ie: we can fallback to a standby machine if one of the machines a process was running on because defunct).

Each of these processes will run locally, and you must kick them off individually. Since the processes need to communicate with each other, the dispatcher and the test runner will run locally listening on distinct local ports.

Files in This Project
---------------------

This project contains python files for each of these components: the repository observer (repo_observer.py), the test job dispatcher (dispatcher.py), and the test runner (test_runner.py). Each of these three processes communicate with each other using sockets, and since the code used to transmit information is shared by all of them, there is a helpers.py file that contains it, so each process imports the 'communicate' function from here instead of having it duplicated in the file.

There are also bash script files used by these processes. These script files are used to execute bash and git commands in an easier way than constantly using python's operating system-level modules like 'os' and 'subprocess'.

Lastly, there is a 'tests' directory, which contains two example tests the CI system will run. One test will pass, and the other will fail.


Initial Setup
--------------

Continuous integration systems run tests whenever a commit is made to a repository. We will need to set up the repository our CI system will monitor.
Let's call this test_repo::

  mkdir test_repo
  git init

This will be our 'master repository'. This is where developers check in their code,
and so our CI should pull this repository and check for commits, then run
tests. The thing that checks for new commits is the repository observer.

The repository observer works by checking commits, so we need at least one commit in
the master repository. Let’s commit our example tests so we have some tests to run.

Copy the tests/ folder from this code base to test_repo and commit it::

  cp -r /this/directory/tests /path/to/test_repo/
  cd /path/to/test_repo
  git add tests/
  git commit -m”add tests”

So now you have a commit in the master repository.

The repo observer will need its own clone of the code, so it can detect when a new commit is made. Let's create a clone of our master repository, and call it test repo_clone_obs::

  git clone /path/to/test_repo test_repo_clone_obs

The test runner will also need its own clone of the code, so it can checkout the repository at a given commit and run the tests. Let's create another clone of our master repository, and call it test_repo_clone_runner::

  git clone /path/to/test_repo test_repo_clone_runner

The Components
==============

The Repository Observer (repo_observer.py)
------------------------------------------

The repository observer must know which repository to observe. To do this, we previously created a clone of our repository at /path/to/test_repo_clone_obs. The repository will use this clone to detect changes. In order for the repository observer to use this clone, when we invoke the repo_observer.py file, we must pass it this path. The repository observer will use this clone to pull from the main repository, and on each commit, will notify the dispatcher.

The repository observer must communicate with the dispatcher, and to do so, it must know its server address and port. When you start the repository observer, you can pass in the dispatcher's server address using the '--dispatcher-server' command line argument. If you do not pass it in, it will assume the default address of 'localhost:8888'. 

Once the repository observer file is invoked, the poll() function is called. This function parses the command line arguments, and then kicks off an infinite while loop. The while loop is used to periodically check the repository for changes. The first thing it does is call the 'update_repo.sh' Bash file. Bash is used because we need to check file existence, create files, and use git, and using a shell script is the most direct and easy way to achieve this. Alternatively, python's 'os' built-in module can be used for using the filesystem and GitPython from PyPI can be used for git access, and these will be operating system independent, but are more roundabout.

The 'update_repo.sh' file is used to identify any new commits and let the repository observer know. It does this by noting what commit hash we are currently aware of, then pulls the repository, and checks the latest commit hash. If they match, no changes are made, so the repository observer doesn't need to do anything, but if there is a difference in the commit hash, then we know a new commit has been made. In this case, 'update_repo.sh' will create a file called .commit_hash with the latest commit hash stored in it.

A step-by-step breakdown of update_repo.sh is as follows. First, the script sources the run_or_fail.sh file, which provides the run_or_fail helper method used by all our shell scripts. This method is used to run the given command, or fail with the given error message. Next, the script tries to remove a file named .commit_hash. Since 'updaterepo.sh' is called infinitely by the repo_observer.py file, if we previously had a new commit, then .commit_hash file was created, but holds a commit we already tested. Threefore we want to remove that file, and create a new one only if a new commit is found. After it removes the file if it existed, it verifies that the repository we are observing exists, and then resets it to the most recent commit, in case anything caused it to get out of sync. It then calls 'git log' and parses the output, looking for the most recent commit hash. Then it pulls the repository, getting any recent changes, then gets the most recent commit hash. Lastly, if the commit hash doesn't match the previous hash, then we know we have new commits to check, so the script stores the latest commit hash in a .commit_hash file.

When 'update_repo.sh' file finishes running in 'repo_observer.py', the repository observer checks for the existence of the .commit_hash file. If it doesn't exist, then the repository observer will sleep for 5 seconds and repeat the process. If the file does exist, then we know we have a new commit, and we need to notify the dispatcher so it can kick off the tests. The repository will try to communicate with the dispatcher server by checking its status first by connecting to it and sending a 'status' request, to make sure there are no problems with the dispatcher server and to make sure it is ready for instruction. If it responds with 'OK', then the repository observer opens the .commit_hash file, reads the latest commit hash and sends that hash to the dispatcher, using a 'dispatch:<commit hash>' request. It will then sleep for 5 seconds and repeat the process.

The repository observer will repeat this process forever, until you kill the process via a KeyboardInterrupt (ctrl+C or cmd+C), or by sending it a kill signal.

The Dispatcher (dispatcher.py)
------------------------------------------

The Test Runner (test_runner.py)
------------------------------------------
    Ideas to explore:
        What the organizational units are (e.g. modules, classes)
        Why things were modularized the way they were
        Points of extensibility
        Tradeoffs: time/space, performance/readability, etc.
        Patterns or principles used that could be applied elsewhere
    Conclusion:
        Further extensions that could be made
        Similar real-world projects to explore


Points of Extensibility
=======================

* Have the test runner or dispatcher report results to a result gathering process, which will host results publicly.

