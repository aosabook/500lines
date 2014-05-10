require_relative 'parser'
require_relative 'user'
require_relative 'device'
require_relative 'analyzer'

class Trial

  attr_reader :file_name, :parser, :user, :device, :analyzer
  attr_reader :user_params, :device_params

  def initialize(file_name)
    @file_name = file_name
  end

  # -- Class Methods --------------------------------------------------------

  def self.all
    file_names = Dir.glob(File.join('public/uploads', "*"))
    file_names.map { |file_name| self.new(file_name) }
  end

  def self.find(file_name)
    self.new(file_name)
  end

  def self.create(data, user_params, device_params)
    p data, user_params, device_params

    @file_name = FileHelper.generate_file_name(@parser, @user, @device)
    cp(file_upload, @file_name)

  end

  # -- Instance Methods -----------------------------------------------------

  def data
    @data ||= File.read(file_name)
  end

  def parser
    @parser ||= Parser.new(data)
  end

  def user
    @user ||= User.new(*file_components.first.split('-'))
  end

  def device
    @device ||= Device.new(*file_components.last.split('-')[0...-1])
  end

  def analyzer
    unless @analyzer
      @analyzer = Analyzer.new(parser, user, device)
      @analyzer.measure
    end
    @analyzer
  end

private

  def file_components
    @file_components ||= file_name.split('/').last.split('_')
  end

end