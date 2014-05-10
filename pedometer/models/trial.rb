
class Trial

  attr_reader :file_name, :data, :user_params, :device_params

  def self.find(file_name)
    self.new(file_name)
  end

  def initialize(file_name)
    @file_name = file_name
  end

  def data
    @data ||= File.read(file_name)
  end

  def user_params
    @user_params ||= file_name.split('/').last.split('_').first.split('-')
  end

  def device_params
    @device_params ||= file_name.split('/').last.split('_').last.split('-')[0...-1]
  end


end