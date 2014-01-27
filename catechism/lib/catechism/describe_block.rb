require "catechism/it_block"

class Catechism::DescribeBlock < Struct.new(:description)
  def it(description, &block)
    Catechism::ItBlock.new(description, self).instance_eval(&block) if block_given?
  end
end
