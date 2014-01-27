module Catechism::Matchers
  class RaiseError < Struct.new(:subject, :negated)
    def raised?
      begin
        subject.call
      rescue StandardError => e
        return true
      end
      false
    end

    def valid?
      raised? ^ negated
    end

    def failure_message
      if negated
        "Expected no error to be raised, but an error was raised"
      else
        "Expected error to be raised, but no error was raised"
      end
    end
  end
end
