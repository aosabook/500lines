#!/usr/bin/env ruby

guard :shell do
  watch(/^trials\/lib\/(.*)_trial\.rb/) {|m| `bundle exec bin/catechism run_trial #{m[0]}` && puts("Passed #{m[0]}") }
  watch(/^trials\/line_trial\.rb/) {|m| `bundle exec bin/catechism run_trial #{m[0]}` && puts("Passed #{m[0]}") }
end
