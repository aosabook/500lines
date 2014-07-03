require 'catechism/method_mocker'

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
      mock_object = Catechism::MethodMocker.new(destination)
      mock_object.expect(method_name, arguments)
      subject.call
      mock_object.cleanup
      mock_object.called? ^ negated
    end

    def failure_message
      if negated
        "Expected #{destination} not to receive #{method_name}, but got it anyway"
      else
        message = "Expected #{method_name} on #{destination}"
        message << " with #{arguments}" unless arguments.nil?
        "#{message}, but was not received"
      end
    end
  end
end
