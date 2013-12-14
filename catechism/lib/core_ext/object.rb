class Object
  def it(description, &block)
    Catechism::ItBlock.new(description).instance_eval(&block) if block_given?
  end
end
