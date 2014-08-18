angular.module(\500lines []).controller \Spreadsheet ($scope, $timeout) ->
  # Begin of $scope properties; start with the column/row labels
  $scope.Cols = [ \A to \H ]
  $scope.Rows = [ 1 to 20 ]

  # UP(38) and DOWN(40)/ENTER(13) move focus to the row above (-1) and below (+1).
  $scope.keydown = ({which}, col, row) ->
    | which in [ 38 40 13 ] => $timeout ->
      const direction = if which is 38 then -1 else +1
      const cell = document.querySelector "##{ col }#{ row + direction }"
      cell?focus!

  # Default sheet content, with some data cells and one formula cell.
  $scope.reset = -> $scope.sheet = { A1: 1874, B1: \+, C1: 2046, D1: \⇒, E1: \=A1+C1 }

  # Define the initializer, and immediately call it
  do $scope.init = ->
    # Restore the previous .sheet; reset to default if it’s the first run
    $scope.sheet = angular.fromJson localStorage.getItem ''
    $scope.reset! unless $scope.sheet
    $scope.worker = new Worker \worker.js

  # Formula cells may produce errors in .errs; normal cell contents are in .vals
  [$scope.errs, $scope.vals] = [ {}, {} ]

  # Define the calculation handler; not calling it yet
  $scope.calc = ->
    const json = angular.toJson $scope.sheet
    const promise = $timeout ->
      # If the worker has not returned in 99 milliseconds, terminate it
      $scope.worker.terminate!
      # Back up to the previous state and make a new worker
      $scope.init!
      # Redo the calculation using the last-known state
      $scope.calc!
    , 99ms

    # When the worker returns, apply its effect on the scope
    $scope.worker.onmessage = ({data}) -> $timeout ->
      [$scope.errs, $scope.vals] = data
      localStorage.setItem '', json
      $timeout.cancel promise

    # Post the current sheet content for the worker to process
    $scope.worker.postMessage $scope.sheet

  # Start calculation when worker is ready
  $scope.worker.onmessage = $scope.calc
  $scope.worker.postMessage null
