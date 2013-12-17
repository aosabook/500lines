module Catechism::Matchers
  class RaiseError < Struct.new(:subject, :error_class, :negated)
    def raised?
      begin
        subject.call
      rescue error_class => e
        return true
      end
      false
    end

    def valid?
      raised? ^ negated
    end

    def failure_message
      if negated
        "Expected no error to be raised, but #{error_class.name} was raised :("
      else
        "Expected #{error_class.name} to be raised, but it was not raised :("
      end
    end
  end
end
