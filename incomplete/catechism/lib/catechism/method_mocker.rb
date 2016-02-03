module Catechism
  class MethodMocker < Struct.new(:original_object)
    attr_reader :method_name
    attr_accessor :called

    def called?
      @called
    end

    def expect(method_name, arguments)
      @method_name = method_name
      this = self
      original_object.send(:define_singleton_method, method_name) do |*args|
        if !arguments.nil?
          this.called = arguments == args
        else
          this.called = true
        end
      end
    end

    def cleanup
      if original_object.respond_to?(method_name)
        removed_method_name = method_name
        original_object.instance_eval { undef :"#{removed_method_name}" }
      end
    end
  end
end