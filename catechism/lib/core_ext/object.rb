class Object
  def describe(description, &block)
    Catechism::DescribeBlock.new(description).instance_eval(&block) if block_given?
  end
end
