var sheet, errs, vals
self.onmessage = ({data}) ->
  [sheet, errs, vals] := [ data, {}, {} ]

  for let coord of sheet
    # Four variable names pointing to the same coordinate: A1, a1, $A1, $a1
    for let name in [ p+c for p in [ '', '$' ] for c in [ coord, coord.toLowerCase! ] ]
      # Worker is reused across calculations, so only define each variable once
      return if Object.getOwnPropertyDescriptor( self, name )?get

      # Define self['A1'], which is the same thing as the global variable A1
      Object.defineProperty self, name, get: ->
        return vals[coord] if coord of vals
        vals[coord] = NaN

        # Turn numeric strings into numbers, so =A1+C1 works when both are numbers
        x = +sheet[coord]
        x = sheet[coord] if sheet[coord] isnt "#x"

        # Evaluate formula cells that begin with =
        try vals[coord] = if \= is x.0 then eval.call null, x.slice 1 else x
        catch
          if e is /\$?[A-Za-z]+[1-9][0-9]*\b/ and that?0 not of self
            # The formula refers to a uninitialized cell; set it to 0 and retry
            self[that.0] = 0
            delete vals[coord]
            return self[coord]
          # Otherwise, stringify the caught exception in the errs object
          errs[coord] = "#e"

        return if typeof vals[coord] is \number then vals[coord] else vals[coord]+=''

  # For each coordinate in the sheet, call the property getter defined above
  for coord of sheet => self[coord]
  postMessage [ errs, vals ]
