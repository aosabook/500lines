require 'catechism/matchers/equal'
require 'catechism/matchers/raise_error'

class Catechism::SubjectWrapper < Struct.new(:subject)
  attr_reader :negated

  def to_equal(expected)
    matcher = Catechism::Matchers::Equal.new(subject, expected, negated)
    raise matcher.failure_message unless matcher.valid?
  end

  def to_be_nil
    matcher = Catechism::Matchers::Equal.new(subject, nil, negated)
    raise matcher.failure_message unless matcher.valid?
  end

  def to_raise_error
    matcher = Catechism::Matchers::RaiseError.new(subject, negated)
    raise matcher.failure_message unless matcher.valid?
  end

  def not
    @negated = true
    self
  end
end
