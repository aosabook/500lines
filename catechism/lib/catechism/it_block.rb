class Catechism::ItBlock < Struct.new(:description, :current_context)
  def expect(subject = nil, &block)
    subject = block if block_given?
    Catechism::SubjectWrapper.new(subject)
  end
end
