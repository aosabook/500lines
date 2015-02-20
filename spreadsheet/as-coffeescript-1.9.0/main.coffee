angular.module("500lines", []).controller "Spreadsheet", ($scope, $timeout) ->
  $scope.Cols = []
  $scope.Rows = []

  for charCode in [65..72]
    char = String.fromCharCode charCode
    $scope.Cols.push char

  for num in [1..20]
    $scope.Rows.push num

  $scope.keydown = (event, col, row) ->
    switch event.which
      when 38, 40, 13
        $timeout ->
          direction = if event.which == 38 then -1 else 1
          cell = document.querySelector('#' + col + (row + direction))
          cell.focus() if cell

  $scope.reset = ->
    $scope.sheet = {}

  ($scope.init = ->
    $scope.sheet = angular.fromJson localStorage.getItem "spreadsheet"
    $scope.reset() unless $scope.sheet
    $scope.worker = new Worker "worker.js"
  )()

  $scope.errs = {}
  $scope.vals = {}

  $scope.calc = ->
    json = angular.toJson $scope.sheet
    promise = $timeout ->
      $scope.worker.terminate()
      $scope.init()
      $scope.calc()
    , 99

    $scope.worker.onmessage = (message) ->
      $timeout.cancel promise
      localStorage.setItem "spreadsheet", json
      $timeout ->
        $scope.errs = message.data[0]
        $scope.vals = message.data[1]

    $scope.worker.postMessage $scope.sheet

  $scope.worker.onmessage = $scope.calc
  $scope.worker.postMessage null
