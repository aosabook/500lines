window.Spreadsheet = ($scope, $timeout)=>{
  function* range(cur, end) { while (cur <= end) {
    yield cur;
    // If it's a number, increase it by one; otherwise move to next letter
    cur = (isNaN( cur ) ? String.fromCodePoint( cur.codePointAt()+1 ) : cur+1);
  } }

  // Begin of $scope properties; start with the column/row labels
  $scope.Cols = [ for (col of range( 'A', 'H' )) col ];
  $scope.Rows = [ for (row of range( 1, 20 )) row ];

  // UP (38) and DOWN/ENTER (40/13) keys move focus to the row above (-1) or below (+1).
  $scope.keydown = ({which}, col, row)=>{ switch (which) {
    case 38: case 40: case 13: $timeout( ()=>{
      const direction = (which == 38) ? -1 : +1;
      const cell = document.querySelector( `#${ col }${ row + direction }` );
      if (cell) { cell.focus(); }
    } )
  } };

  // Default sheet content, with some data cells and one formula cell.
  $scope.reset = ()=>{ $scope.sheet = { A1: 1874, B1: '+', C1: 2046, D1: '⇒', E1: '=A1+C1' } };

  // Define the initializer, and immediately call it
  ($scope.init = ()=>{
    // Restore the previous .sheet; reset to default if it's the first run
    $scope.sheet = angular.fromJson( localStorage.getItem( '' ) );
    if (!$scope.sheet) { $scope.reset(); }
    $scope.worker = new Worker( 'worker.js' );
  })();

  // Formula cells may produce errors in .errs; normal cell contents are in .vals
  [$scope.errs, $scope.vals] = [ {}, {} ];

  // Define the calculation handler, and immediately call it
  ($scope.calc = ()=>{
    const json = angular.toJson( $scope.sheet );
    const promise = $timeout( ()=>{
      // If the worker has not returned in 0.5 seconds, terminate it
      $scope.worker.terminate();
      // Back up to the previous state and make a new worker
      $scope.init();
      // Redo the calculation using the last-known state
      $scope.calc();
    }, 500 );

    // When the worker returns, apply its effect on the scope
    $scope.worker.onmessage = ({data})=>{ $timeout( ()=>{
      [$scope.errs, $scope.vals] = data;
      localStorage.setItem( '', json );
      $timeout.cancel( promise );
    } ) }

    // Post the current sheet content for the worker to process
    $scope.worker.postMessage( $scope.sheet );
  })();
}
