module Catechism::Matchers
  class Send < Struct.new(:subject, :method_name, :negated)
    attr_reader :destination, :arguments

    def to(destination)
      @destination = destination
      raise failure_message unless valid?
    end

    def with(*arguments)
      @arguments = arguments
      self
    end

    def valid?
      called = false
      send_matcher = self
      destination.class.send(:define_method, method_name) do |*args|
        if !send_matcher.arguments.nil?
          called = send_matcher.arguments == args
        else
          called = true
        end
      end
      subject.call
      called ^ negated
    end

    def failure_message
      if negated
        "Expected #{destination} not to receive #{method_name}, but got it anyway"
      else
        "Expected #{method_name} on #{destination}, but was not received"
      end
    end
  end
end
