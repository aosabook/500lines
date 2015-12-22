React = require \react
{table, thead, tbody, tr, th, td, input, div, button} = React.DOM

SheetDefault = { A1: 1874, B1: \+, C1: 2046, D1: \⇒, E1: \=A1+C1 }
SheetInit = (try JSON.parse localStorage.getItem '') || SheetDefault

Table = React.createFactory React.createClass do
  displayName: \Table
  getDefaultProps: -> { Cols: [\A to \H], Rows: [1 to 20] }
  getInitialState: -> { sheet: SheetInit, vals: {}, errs: {} }
  render: ->
    {Cols, Rows} = @props
    table {},
      thead {}, tr {},
        th {}, button { type: \button onClick: @~reset } \↻
        ...[ th {}, col for col in Cols ]
      tbody {},
        ...[ Row { Cols, row, @~onChange } <<< @state for row in Rows ]
  reset: -> @calc SheetDefault
  componentDidMount: -> @calc @state.sheet
  calc: (sheet) ->
    const worker = @state.worker ||= @props.worker
    const timeout = setTimeout ~>
      # If the worker has not returned in 99 milliseconds, terminate it
      worker.terminate!
      @setState { worker: new Worker \worker.js }
    , 99ms

    # When the worker returns, apply its effect on the scope
    worker.onmessage = ({data: [errs, vals]}) ~>
      clearTimeout timeout
      localStorage.setItem '', JSON.stringify sheet
      @setState { sheet, errs, vals }
    worker.postMessage sheet
  onChange: ({target: {id, value}}) ->
    sheet = {} <<< @state.sheet
    @calc(sheet <<< { "#id": value })

Row = React.createFactory React.createClass do
  displayName: \Row
  render: ->
    {Cols, sheet, vals, errs, row, onChange} = @props
    tr {},
      th {}, row
      ...for let col in Cols
        id = col + row
        onKeyDown = @~onKeyDown _, col, row
        Cell { id, onChange, onKeyDown, txt: sheet[id], err: errs[id], val: vals[id] }
  onKeyDown: ({target, key}, col, row) ->
    | key in <[ ArrowUp ArrowDown Enter ]> =>
      const direction = if key is \ArrowUp then -1 else +1
      const cell = document.querySelector "##{ col }#{ row + direction }"
      cell?focus!

Cell = React.createFactory React.createClass displayName: \Cell render: ->
  {id, txt, err, val, onChange, onKeyDown} = @props
  td { className: if txt?0 is \= then \formula else '' },
    input { id, type: \text, value: txt, onChange, onKeyDown }
    div { className: if err then \error else if val?0 then \text else '' } (err || val)

window.init = ->
  worker = new Worker \worker.js
  worker.onmessage = -> React.render Table({ worker }), document.body
  worker.postMessage null
