angular.module('500lines', []).controller('Spreadsheet', function($scope, $timeout) {
  // Begin of $scope properties; start with the column/row labels
  $scope.Cols = [], $scope.Rows = [];
  makeRange($scope.Cols, 'A', 'H');
  makeRange($scope.Rows, 1, 20);
  function makeRange(array, cur, end) { while (cur <= end) { array.push(cur);
    // If it’s a number, increase it by one; otherwise move to next letter
    cur = (isNaN( cur ) ? String.fromCharCode( cur.charCodeAt()+1 ) : cur+1);
  } }

  // UP(38) and DOWN(40)/ENTER(13) move focus to the row above (-1) and below (+1).
  $scope.keydown = function(event, col, row) { switch (event.which) {
    case 38: case 40: case 13: $timeout( function() {
      var direction = (event.which === 38) ? -1 : +1;
      var cell = document.querySelector( '#' + col + (row + direction) );
      if (cell) { cell.focus(); }
    } );
  } };

  // Default sheet content, with some data cells and one formula cell.
  $scope.reset = function() { $scope.sheet = { A1: 1874, B1: '+', C1: 2046, D1: '⇒', E1: '=A1+C1' }; };

  // Define the initializer, and immediately call it
  ($scope.init = function() {
    // Restore the previous .sheet; reset to default if it’s the first run
    $scope.sheet = angular.fromJson( localStorage.getItem( '' ) );
    if (!$scope.sheet) { $scope.reset(); }
    $scope.worker = new Worker( 'worker.js' );
  }).call();

  // Formula cells may produce errors in .errs; normal cell contents are in .vals
  $scope.errs = {}, $scope.vals = {};

  // Define the calculation handler; not calling it yet
  $scope.calc = function() {
    var json = angular.toJson( $scope.sheet );
    var promise = $timeout( function() {
      // If the worker has not returned in 99 milliseconds, terminate it
      $scope.worker.terminate();
      // Back up to the previous state and make a new worker
      $scope.init();
      // Redo the calculation using the last-known state
      $scope.calc();
    }, 99 );

    // When the worker returns, apply its effect on the scope
    $scope.worker.onmessage = function(message) {
      $timeout.cancel( promise );
      localStorage.setItem( '', json );
      $timeout( function() { $scope.errs = message.data[0], $scope.vals = message.data[1]; } );
    };

    // Post the current sheet content for the worker to process
    $scope.worker.postMessage( $scope.sheet );
  };

  // Start calculation when worker is ready
  $scope.worker.onmessage = $scope.calc;
  $scope.worker.postMessage( null );
});
