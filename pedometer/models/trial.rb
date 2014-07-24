require 'fileutils'
require_relative 'analyzer'

include FileUtils::Verbose

class Trial

  attr_reader :file_name, :processor, :user, :device, :analyzer
  attr_reader :user_params, :device_params

  def initialize(file_name = nil, input_data = nil, user_params = nil, device_params = nil)
    if file_name
      @file_name = file_name
    elsif input_data
      @processor = Processor.new(File.read(input_data))
      @user   = User.new(*user_params)
      @device = Device.new(*device_params)

      @file_name = "public/uploads/" + 
                   "#{user.gender}-#{user.height}-#{user.stride}_" +
                   "#{device.rate}-" + 
                   "#{device.steps}-" +
                   "#{device.trial.to_s.gsub(/\s+/, '')}-" + 
                   "#{device.method}-#{processor.format[0]}.txt"
    else 
      raise 'File name or input data must be passed in.'
    end
  end

  # -- Class Methods --------------------------------------------------------

  def self.create(input_data, user_params, device_params)
    trial = self.new(nil, input_data, user_params, device_params)
    cp(input_data, trial.file_name)
    trial
  end

  def self.find(file_name)
    self.new(file_name)
  end

  def self.all
    file_names = Dir.glob(File.join('public/uploads', "*"))
    file_names.map { |file_name| self.new(file_name) }
  end

  # -- Instance Methods -----------------------------------------------------

  def processor
    @processor ||= Processor.new(File.read(file_name))
  end

  def user
    @user ||= User.new(*file_components.first.split('-'))
  end

  def device
    @device ||= Device.new(*file_components.last.split('-')[0...-1])
  end

  def analyzer
    unless @analyzer
      @analyzer = Analyzer.new(processor, user, device)
      @analyzer.measure
    end
    @analyzer
  end

private

  def file_components
    @file_components ||= file_name.split('/').last.split('_')
  end

end