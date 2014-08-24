What is a Continuous Integration System?
========================================

When developing software, we want to be able to verify that our new features or bugfixes are safe and work as expected. We do this by running tests against our code. Sometimes, developers will run tests locally to verify that their changes are safe, but developer smay not have the time to test their code on every system their software runs in, and when more and more tests get added, the amount of time to run them locally increases, and it becomes increasingly inefficient. Because of this, continuous integration systems have been created.

Continuous Integration (CI) systems are dedicated systems used to test new code. Upon a commit to the code repository, it is the responsibility of the continuous integration system to verify that this commit will not break any tests. To do this, the system must be able to fetch the new changes, run the tests and report its results. Like any other system, it should also be failure resistant. This means if any part of the system fails, it should be able to recover and continue from that point. This test system should also handle load well, so that we can get test results in a reasonable about of time in the event that commits are being made faster than the tests can be run. We can achieve this by distributing and parallelizing the testing effort. This project will demonstrate a small-scale, distributed continuous integration system. It is a bare-bones system, but is designed from extensibility.


Project Limitations
-------------------

This project uses Git as the code repository that needs to be tested. Only standard source code management calls will be used, so if you are unfamiliar with Git but are familiar with other version control systems like svn or Mercurial, you can still follow along.

This project also assumes you are using a POSIX system.

Due to the limitations of code length and unittest, I simplified test
discovery. We will *only* run tests that are in a directory named "tests" within
the repository.

Continuous integration systems need not run per-commit. You can also have them run every few commits, or periodically. For our example case, I am simplifying this to just being per-commit.

This CI system is designed to check periodically for changes in a repository. In real-world CI systems, you can also have the repository observer get notified by a hosted repository. Github, for example, provides 'post-commit hooks' which send out notifications to a URL. Following this model, the repository observer would be called by the webserver hosted at that URL to respond to that notification. Since this is complex to model locally, we're using an observer model, where the repository observer will check for changes instead of being notified.

Introduction
------------

The basic continuous integration (CI) system has 3 components: a listener or watcher for changes in the repository, a test job dispatcher, and a test runner. The first component is used to monitor the repository. When it notices that a commit has been made, then it must notify the job dispatcher. The job dispatcher then finds a test runner and gives it the commit number to test. In this project, each of these components is its own process, and can be run on separate machines if you wish. In real world systems, they are run in a distributed environment so we can have failover redundancy (ie: we can fallback to a standby machine if one of the machines a process was running on because defunct).

Files in this project
---------------------

This project contains python files for each of these components: the repository observer (repo_observer.py), the test job dispatcher (dispatcher.py), and the test runner (test_runner.py). Each of these three processes communicate with each other using sockets, and since the code used to transmit information is shared by all of them, there is a helpers.py file that contains it, so each process imports the 'communicate' function from here instead of having it duplicated in the file.

There are also bash script files used by these processes. These script files are used to execute bash and git commands in an easier way than constantly using python's operating system-level modules like 'os' and 'subprocess'.

Lastly, there is a 'tests' directory, which contains two example tests the CI system will run. One test will pass, and the other will fail.


Initial Set-up
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


The repository observer (repo_observer.py)
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

