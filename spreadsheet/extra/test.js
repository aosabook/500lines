#!/usr/bin/env node
'use strict'
var Worker = this.Worker || require('webworker-threads').Worker
var w = new Worker('dist/worker.js')
w.onmessage = function (event) {
  console.log(event.data)
  if (this.Worker) { return }
  require('assert').deepEqual(
    event.data, [ {}, { A1: -579, A2: 71217, B1: 123, B2: 456 } ]
  )
  console.log("ok 1")
  process.exit()
}
w.postMessage({
  A1: '=-B1-B2',
  A2: '=$B1*-$A1+C9-J19',
  B1: 123,
  B2: 456
})
