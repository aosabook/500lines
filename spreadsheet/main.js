'use strict'
window.Spreadsheet = ($scope)=>{
  function* range(cur, end) { while (cur <= end) {
    yield cur;
    cur = (isNaN(cur) ? String.fromCodePoint(cur.codePointAt()+1) : cur+1)
  } }
  $scope.Cols = [for (col of range('A', 'H')) col]
  $scope.Rows = [for (row of range(1, 20)) row]
  $scope.sheet = angular.fromJson( localStorage.getItem( 'sheet' ) )
              || { A1: 1874, B1: '✕', C1: 2046, D1: '⇒', E1: '=A1*C1' }
  $scope.errs = {}, $scope.vals = {}
  const worker = new Worker( 'dist/worker.js' )
  worker.onmessage = (event)=>{ $scope.$apply( ()=>{ [$scope.errs, $scope.vals] = event.data } ) }
  let cache = ''
  $scope.calc = ()=>{
    const json = angular.toJson( $scope.sheet )
    if (json === cache) { return }
    localStorage.setItem( 'sheet', ( cache = json ) )
    worker.postMessage( $scope.sheet )
  }
  $scope.calc()
}
