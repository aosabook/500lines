window.Spreadsheet = ($scope)=>{
  function* range(cur, end) { while (cur <= end) {
    yield cur
    cur = (isNaN(cur) ? String.fromCodePoint(cur.codePointAt()+1) : cur+1)
  } }
  $scope.Cols = [ for (col of range('A', 'H')) col ]
  $scope.Rows = [ for (row of range(1, 20)) row ]
  $scope.press = ({which, target}, col, row)=>{ switch (which) {
    case 38: case 40: (document.getElementById(`${ col }${ row + which - 39 }`) || target).focus()
  } }
  $scope.reset = ()=>{ $scope.sheet = { A1: 1874, B1: '✕', C1: 2046, D1: '⇒', E1: '=A1*C1' } }
  ($scope.sheet = angular.fromJson( localStorage.getItem( 'sheet' ) )) || $scope.reset()
  $scope.errs = {}, $scope.vals = {}
  const worker = new Worker( 'dist/worker.js' )
  worker.onmessage = (event)=>{ $scope.$apply( ()=>{ [$scope.errs, $scope.vals] = event.data } ) }
  ($scope.calc = ()=>{
    const json = angular.toJson( $scope.sheet )
    if (json === $scope.cache) { return }
    localStorage.setItem( 'sheet', ( $scope.cache = json ) )
    worker.postMessage( $scope.sheet )
  })()
}
