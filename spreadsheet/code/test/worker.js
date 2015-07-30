var Worker = this.Worker || require('webworker-threads').Worker
var w = new Worker('as-javascript-1.8.5/worker.js')
var assert = require("assert")
describe('Worker', function(){
  it('should support formula', function(done){
    w.onmessage = function (event) {
      assert.deepEqual(
        event.data, [ {}, { A1: -579, A2: 71217, B1: 123, B2: 456 } ]
      );
      done();
    }
    w.postMessage({
      A1: '=-B1-B2',
      A2: '=$B1*-$A1+C9-J19',
      B1: 123,
      B2: 456
    })
  })
})

