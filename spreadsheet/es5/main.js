angular.module('500lines', []).controller('Spreadsheet', function($scope, $timeout) {
  var $__8 = $traceurRuntime.initGeneratorFunction(range);
  var $__4;
  $scope.Cols = [], $scope.Rows = [];
  for (var $__0 = range('A', 'H')[Symbol.iterator](),
      $__1; !($__1 = $__0.next()).done; ) {
    col = $__1.value;
    {
      $scope.Cols.push(col);
    }
  }
  for (var $__2 = range(1, 20)[Symbol.iterator](),
      $__3; !($__3 = $__2.next()).done; ) {
    row = $__3.value;
    {
      $scope.Rows.push(row);
    }
  }
  function range(cur, end) {
    return $traceurRuntime.createGeneratorInstance(function($ctx) {
      while (true)
        switch ($ctx.state) {
          case 0:
            $ctx.state = (cur <= end) ? 1 : -2;
            break;
          case 1:
            $ctx.state = 2;
            return cur;
          case 2:
            $ctx.maybeThrow();
            $ctx.state = 4;
            break;
          case 4:
            cur = (isNaN(cur) ? String.fromCodePoint(cur.codePointAt() + 1) : cur + 1);
            $ctx.state = 0;
            break;
          default:
            return $ctx.end();
        }
    }, $__8, this);
  }
  $scope.keydown = (function($__4, col, row) {
    var which = $__4.which;
    switch (which) {
      case 38:
      case 40:
      case 13:
        $timeout((function() {
          var direction = (which === 38) ? -1 : +1;
          var cell = document.querySelector(("#" + col + (row + direction)));
          if (cell) {
            cell.focus();
          }
        }));
    }
  });
  $scope.reset = (function() {
    $scope.sheet = {
      A1: 1874,
      B1: '+',
      C1: 2046,
      D1: '⇒',
      E1: '=A1+C1'
    };
  });
  ($scope.init = (function() {
    $scope.sheet = angular.fromJson(localStorage.getItem(''));
    if (!$scope.sheet) {
      $scope.reset();
    }
    $scope.worker = new Worker('worker.js');
  })).call();
  ($__4 = [{}, {}], $scope.errs = $__4[0], $scope.vals = $__4[1], $__4);
  $scope.calc = (function() {
    var json = angular.toJson($scope.sheet);
    var promise = $timeout((function() {
      $scope.worker.terminate();
      $scope.init();
      $scope.calc();
    }), 99);
    $scope.worker.onmessage = (function($__5) {
      var data = $__5.data;
      $timeout.cancel(promise);
      localStorage.setItem('', json);
      $timeout((function() {
        var $__7;
        ($__7 = data, $scope.errs = $__7[0], $scope.vals = $__7[1], $__7);
      }));
    });
    $scope.worker.postMessage($scope.sheet);
  });
  $scope.worker.onmessage = $scope.calc;
  $scope.worker.postMessage(null);
});

//# sourceMappingURL=main.map
