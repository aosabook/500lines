module Catechism
  module Matchers
    class Equal < Struct.new(:subject, :expectation, :negated)
      def equal?
        subject == expectation
      end

      def valid?
        equal? ^ negated
      end

      def failure_message
        if negated
          "#{subject.inspect} equals #{expectation.inspect}"
        else
          "#{subject.inspect} does not equal #{expectation.inspect}"
        end
      end
    end
  end
end
