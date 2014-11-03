require 'fileutils'
require_relative 'user'
require_relative 'trial'

include FileUtils::Verbose

class Upload

  UPLOAD_DIRECTORY = 'public/uploads/'

  attr_reader :file_path, :user, :trial

  def initialize(file_path)
    @file_path = file_path
    file_name = @file_path.split('/').last.split('.txt').first.split('_')
    @user = User.new(*file_name.first.split('-'))
    @trial = Trial.new(*file_name.last.split('-'))
  end

  # -- Class Methods --------------------------------------------------------

  def self.generate_file_path(user, trial)
    UPLOAD_DIRECTORY + "#{user.gender}-#{user.height}-#{user.stride}_" +
                       "#{trial.name.to_s.gsub(/\s+/, '')}-" + 
                       "#{trial.rate}-#{trial.steps}-#{trial.method}.txt"
  end

  def self.create(temp_file, user, trial)
    cp(temp_file, self.generate_file_path(user, trial))
  end

  def self.find(file_path)
    self.new(file_path)
  end

  def self.all
    file_paths = Dir.glob(File.join(UPLOAD_DIRECTORY, "*"))
    file_paths.map { |file_path| self.new(file_path) }
  end

end