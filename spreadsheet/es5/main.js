function Spreadsheet($scope, $timeout) {
  var $__7 = $traceurRuntime.initGeneratorFunction(range);
  var $__4;
  $scope.Cols = (function() {
    var $__0 = 0,
        $__1 = [];
    for (var $__2 = range('A', 'H')[Symbol.iterator](),
        $__3; !($__3 = $__2.next()).done; ) {
      try {
        throw undefined;
      } catch (col) {
        {
          col = $__3.value;
          $__1[$__0++] = col;
        }
      }
    }
    return $__1;
  }());
  $scope.Rows = (function() {
    var $__0 = 0,
        $__1 = [];
    for (var $__2 = range(1, 20)[Symbol.iterator](),
        $__3; !($__3 = $__2.next()).done; ) {
      try {
        throw undefined;
      } catch (row) {
        {
          row = $__3.value;
          $__1[$__0++] = row;
        }
      }
    }
    return $__1;
  }());
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
    }, $__7, this);
  }
  $scope.keydown = (function($__4, col, row) {
    var which = $traceurRuntime.assertObject($__4).which;
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
      var data = $traceurRuntime.assertObject($__5).data;
      $timeout.cancel(promise);
      localStorage.setItem('', json);
      $timeout((function() {
        var $__6;
        ($__6 = $traceurRuntime.assertObject(data), $scope.errs = $__6[0], $scope.vals = $__6[1], $__6);
      }));
    });
    $scope.worker.postMessage($scope.sheet);
  });
  $scope.worker.onmessage = $scope.calc;
  $scope.worker.postMessage(null);
}

//# sourceMappingURL=main.map
