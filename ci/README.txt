Author: Malini Das
Project: SuperSimpleCI
Requirements: Posix, Python, Git

This is a simplified version of a distributed continuous integration system. 
-It assumes you're running against a git repository, and needs git on the PATH.
Start the dispatcher first, then the test_runner (so it can register itself) and then the repo_observer. Results will be in a test_results/ folder.

* repo_observer.py -- Checks the repo for changes and notifies the dispatcher
* dispatcher.py -- Receives test requests and dispatches them against test runners
* test_runner.py -- Runs the tests and returns the results 
* helpers.py -- Holds shared code
* update_repo.sh -- Updates the shared repo and drops a new file with the hash if there's a change
