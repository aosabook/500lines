{table, thead, tbody, tr, th, td, input, div, button} = React.DOM

SheetDefault = { A1: 1874, B1: \+, C1: 2046, D1: \⇒, E1: \=A1+C1 }
SheetInit = (try JSON.parse localStorage.getItem '') || SheetDefault

Table = React.createClass do
  getDefaultProps: -> { Cols: [\A to \H], Rows: [1 to 20] }
  getInitialState: -> { sheet: SheetInit, vals: {}, errs: {} }
  render: -> table null,
    thead null, tr null,
      th null, button { type: \button onClick: ~> @reset! } \↻
      ...[ th null, col for col in @props.Cols ]
    tbody null,
      ...[ Row {
        row: row,
        Cols: @props.Cols,
        onChange: ~> @onChange ...
      } <<< @state for row in @props.Rows ]
  reset: -> @calc SheetDefault
  componentDidMount: -> @calc @state.sheet
  calc: (sheet) ->
    const worker = @props.worker
    const timeout = setTimeout ~>
      # If the worker has not returned in 99 milliseconds, terminate it
      worker.terminate!
      @setProps { worker: new Worker \worker.js }
    , 99ms

    # When the worker returns, apply its effect on the scope
    worker.onmessage = ({data: [errs, vals]}) ~>
      clearTimeout timeout
      localStorage.setItem '', JSON.stringify sheet
      @setState { sheet, errs, vals }
    worker.postMessage sheet
  onChange: ({ target: {id, value} }) ->
    sheet = {} <<< @state.sheet
    sheet[id] = value
    @calc sheet

Row = React.createClass do
  render: ->
    { Cols, sheet, vals, errs, row, onChange } = @props
    tr null,
      th(, row)
      ...[ Cell {
        id: col+row, col: col,
        txt: sheet[col+row], err: errs[col+row], val: vals[col+row]
        onChange: onChange, onKeyDown: ~> @onKeyDown(...)
      } for col in Cols ]
  onKeyDown: ({ target, which }, col) ->
    | which in [ 38 40 13 ] =>
      const direction = if which is 38 then -1 else +1
      const cell = document.querySelector "##{ col }#{ @props.row + direction }"
      cell?focus!

Cell = React.createClass render: ->
  { id, col, txt, err, val, onChange, onKeyDown } = @props
  td { className: if txt is /^=/ then \formula else '' },
    input { id, type: \text, value: txt, onChange, onKeyDown: -> onKeyDown(it, col) },
    div { className: if err then \error else if val?0 then \text else '' } (err || val)

window.init = ->
  worker = new Worker \worker.js
  worker.onmessage = -> React.renderComponent Table({ worker }), document.body
  worker.postMessage null
