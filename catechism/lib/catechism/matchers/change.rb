module Catechism::Matchers
  class Change < Struct.new(:operation, :negated, :subject)
    attr_reader :expected_difference, :computed_difference

    def by(expected_difference)
      @expected_difference = expected_difference
      raise failure_message unless valid?
    end

    def valid?
      original_value = subject.call
      operation.call
      value_after_operation = subject.call
      @computed_difference = value_after_operation - original_value
      (computed_difference == expected_difference) ^ negated
    end

    def failure_message
      if negated
        "Expected subject not to change, but it changed by #{computed_difference}"
      else
        "Expected subject to change by #{expected_difference}, but it changed by #{computed_difference}"
      end
    end
  end
end
