sheet = {}
errs = {}
vals = {}

@onmessage = (message) ->
  sheet = message.data

  Object.getOwnPropertyNames(sheet || {}).forEach (coord) ->
    ['', '$'].forEach (p) ->
      [coord, coord.toLowerCase()].forEach (c) ->
        name = p + c
        return if (Object.getOwnPropertyDescriptor(@, name) || {}).get
        Object.defineProperty @, name,
          get: ->
            return vals[coord] if coord in vals
            vals[coord] = NaN

            x = +sheet[coord]
            x = sheet[coord] if sheet[coord] != x.toString()

            try
              vals[coord] = if '=' == x[0] then eval.call(null, x.slice(1)) else x
            catch e
              match = /\$?[A-Za-z]+[1-9][0-9]*\b/.exec e
              if match && match[0] in @
                @[match[0]] = 0
                delete vals[coord]
                return @[coord]

            switch typeof vals[coord]
              when "function", "object"
                vals[coord] += ''

            vals[coord]

  Object.keys(sheet).forEach (coord) ->
    @[coord]

  postMessage [errs, vals]
