require 'fileutils'
require_relative 'user'
require_relative 'trial'

include FileUtils::Verbose

class Upload

  UPLOAD_DIRECTORY = 'public/uploads/'

  attr_reader :file_path, :user, :trial

  def initialize(file_path = nil, user_params = nil, trial_params = nil)
    if @file_path = file_path
      file_name = @file_path.split('/').last.split('.txt').first.split('_')
      @user     = User.new(*file_name.first.split('-'))
      @trial    = Trial.new(*file_name.last.split('-'))
    elsif user_params && trial_params
      @user      = User.new(*user_params.values)
      @trial     = Trial.new(*trial_params.values)
      @file_path = Upload.generate_file_path(user, trial)
    else
      raise 'A file path or user and trial parameters must be provided.'
    end
  end

  def self.create(temp_file, user_params, trial_params)
    upload = self.new(nil, user_params, trial_params)
    cp(temp_file, upload.file_path)
    upload
  end

  def self.find(file_path)
    self.new(file_path)
  end

  def self.all
    file_paths = Dir.glob(File.join(UPLOAD_DIRECTORY, "*"))
    file_paths.map { |file_path| self.new(file_path) }
  end

  def self.generate_file_path(user, trial)
    UPLOAD_DIRECTORY +
    "#{user.gender}-#{user.height}-#{user.stride}_" +
    "#{trial.name}-#{trial.rate}-#{trial.steps}.txt"
  end

end
