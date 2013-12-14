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
          "#{subject} equals #{expected}"
        else
          "#{subject} does not equal #{expected}"
        end
      end
    end
  end
end
