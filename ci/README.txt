Author: Malini Das
Project: SuperSimpleCI
Requirements: Posix, Python, Git

This is a simplified version of a distributed continuous integration system. 
It assumes you're running against a git repository, and needs git on the PATH.

To get the example working, start a local git repository in a separate folder. Let's call this test_repo.

Then start the dispatcher first:
    python dispatcher.py --port=8888


Then the test_runner (so it can register itself with the dispatcher):

    python test_runner.py --dispatcher-server=localhost:8888

Then the repo_observer:

    python repo_observer.py ---dispatcher-server=localhost:8888 <path/to/test_repo>

Copy the tests/ directory to test_repo and then commit that change. You should see output in the python scripts, and a test will run. Results will be in a test_results/ folder.

* repo_observer.py -- Checks the repo for changes and notifies the dispatcher
* dispatcher.py -- Receives test requests and dispatches them against test runners
* test_runner.py -- Runs the tests and returns the results 
* helpers.py -- Holds shared code
* update_repo.sh -- Updates the shared repo and drops a new file with the hash if there's a change
