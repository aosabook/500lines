require "catechism/it_block"

class Catechism::DescribeBlock < Struct.new(:description, :block)
  def before(&block)
    before_blocks << block if block_given?
  end

  def after(&block)
    after_blocks << block if block_given?
  end

  def call
    instance_eval(&block)
  end

  def it(description, &block)
    it_block = Catechism::ItBlock.new(description, self)
    before_blocks.each { |before_block| it_block.instance_eval(&before_block) }
    it_block.instance_eval(&block) if block_given?
    after_blocks.each { |after_block| it_block.instance_eval(&after_block) }
  end

  def before_blocks
    @before_blocks ||= []
  end

  def after_blocks
    @after_blocks ||= []
  end
end
