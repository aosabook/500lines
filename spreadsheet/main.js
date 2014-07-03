window.Spreadsheet = ($scope)=>{
  function* range(cur, end) { while (cur <= end) {
    yield cur;
    // If it's a number, increase it by one; otherwise move to next letter
    cur = (isNaN( cur ) ? String.fromCodePoint( cur.codePointAt()+1 ) : cur+1);
  } }

  // Begin of $scope properties; start with the column/row labels
  $scope.Cols = [ for (col of range( 'A', 'H' )) col ];
  $scope.Rows = [ for (row of range( 1, 20 )) row ];

  // Default sheet content, with some data cells and one formula cell.
  $scope.reset = ()=>{ $scope.sheet = { A1: 1874, B1: '✕', C1: 2046, D1: '⇒', E1: '=A1*C1' } };

  // Define the initializer, and immediately call it
  ($scope.init = ()=>{
    // Restore the previous .sheet; reset to default if it's the first run
    $scope.sheet = angular.fromJson( localStorage.getItem( '' ) );
    if (!$scope.sheet) { $scope.reset(); }
    $scope.worker = new Worker( 'dist/worker.js' );
  })();

  // Formula cells may produce errors in .errs; normal cell contents are in .vals
  $scope.errs = {};
  $scope.vals = {};

  // UP (38) and DOWN (40) keys move focus to the row above (-1) or below (+1).
  $scope.keydown = ({which, target}, col, row)=>{ switch (which) {
    case 38: case 40: (document.querySelector( `#${ col }${ which-39+row }` ) || target).focus()
  } };

  // Define the calculation handler, and immediately call it
  ($scope.calc = ()=>{
    const json = angular.toJson( $scope.sheet );
    const timeout = setTimeout( ()=>{
      // If the worker has not returned in 0.5 seconds, terminate it
      $scope.worker.terminate();
      // Back up to the previous state and make a new worker
      $scope.$apply( ()=>{ $scope.init() } );
    }, 500 );
    $scope.worker.onmessage = ({data})=>{ $scope.$apply( ()=>{
      [$scope.errs, $scope.vals] = data;
      localStorage.setItem( '', json );
      clearTimeout( timeout );
    } ) }
    $scope.worker.postMessage( $scope.sheet );
  })();
}
