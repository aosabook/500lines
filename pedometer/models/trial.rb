require 'fileutils'
require_relative 'parser'
require_relative 'user'
require_relative 'device'
require_relative 'analyzer'

include FileUtils::Verbose

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

  def self.create(input_data, user_params, device_params)
    parser = Parser.new(File.read(input_data))
    user   = User.new(*user_params)
    device = Device.new(*device_params)

    file_name = 
      "public/uploads/#{user.gender}-#{user.height}-#{user.stride}_" +
      "#{device.rate}-" + 
      "#{device.steps}-" +
      "#{device.trial.to_s.gsub(/\s+/, '')}-" + 
      "#{device.method}-#{parser.format[0]}.txt"

    cp(input_data, file_name)
    self.new(file_name)
  end

  # -- Instance Methods -----------------------------------------------------

  def data
    @data ||= File.read(file_name)
  end

  def parser
    @parser ||= Parser.new(data)
  end

  def user
    params = file_components.first.split('-')
    @user ||= User.new(*params)
  end

  def device
    params = file_components.last.split('-')[0...-1]
    @device ||= Device.new(*params)
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