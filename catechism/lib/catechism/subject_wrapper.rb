require 'catechism/matchers/equal'
require 'catechism/matchers/raise_error'
require 'catechism/matchers/send'
require 'catechism/matchers/change'

class Catechism::SubjectWrapper < Struct.new(:subject)
  attr_reader :negated

  def to_equal(expected)
    Catechism::Matchers::Equal.new(subject, expected, negated)
  end

  def to_be_nil
    Catechism::Matchers::Equal.new(subject, nil, negated)
  end

  def to_raise_error(error_class = StandardError)
    Catechism::Matchers::RaiseError.new(subject, error_class, negated)
  end

  def to_send(method_name)
    Catechism::Matchers::Send.new(subject, method_name, negated)
  end

  def to_change(&block)
    Catechism::Matchers::Change.new(subject, negated, block)
  end

  def not
    @negated = true
    self
  end
end
