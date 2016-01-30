module Catechism
  module Matchers
    class Equal < Struct.new(:subject, :expectation, :negated)
      def initialize(subject, expectation, negated)
        super(subject, expectation, negated)
        raise failure_message unless valid?
      end
      
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
