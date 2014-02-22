'use strict'
window.Spreadsheet = ($scope)=>{
  [ $scope.Cols, $scope.Rows ] = [ 'ABCDEFG', '123456789' ]
  $scope.errs = {}
  $scope.vals = {}
  $scope.sheet = { A1: 1874, B1: '✕', C1: 2046, D1: '⇒', E1: '=A1*C1' }
  const worker = new Worker( 'dist/worker.js' )
  worker.onmessage = (event)=>{ $scope.$apply( ()=>{ [$scope.errs, $scope.vals] = event.data } ) }
  let cached = ''
  $scope.calc = ()=>{
    const json = angular.toJson( $scope.sheet )
    if (json === cached) { return }
    worker.postMessage( $scope.sheet )
    cached = json
  }
  $scope.calc()
}
