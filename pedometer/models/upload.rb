require 'fileutils'
require_relative 'analyzer'

include FileUtils::Verbose

class Upload

  UPLOAD_DIRECTORY = 'public/uploads/'

  attr_reader :file_path, :user_params, :trial_params

  def initialize(file_path)
    @file_path = file_path
  end

  # -- Class Methods --------------------------------------------------------

  def self.generate_file_path_temp(user_params, trial_params)
    user_file_path = user_params.join('-')
    trial_file_path = trial_params.map { |x| x.to_s.gsub(/\s+/, '')}.join('-')

    UPLOAD_DIRECTORY + "#{user_file_path}_#{trial_file_path}.txt"
  end

  # TODO: Get rid of this and use the one above
  def self.generate_file_path(analyzer)
    UPLOAD_DIRECTORY + "#{analyzer.user.gender}-" + 
                       "#{analyzer.user.height}-" + 
                       "#{analyzer.user.stride}_" +
                       "#{analyzer.trial.name.to_s.gsub(/\s+/, '')}-" + 
                       "#{analyzer.trial.rate}-" + 
                       "#{analyzer.trial.steps}-" +
                       "#{analyzer.trial.method}.txt"
  end

  def self.create(temp_file, analyzer)
    cp(temp_file, self.generate_file_path(analyzer))
  end

  def self.find(file_path)
    self.new(file_path)
  end

  def self.all
    file_paths = Dir.glob(File.join(UPLOAD_DIRECTORY, "*"))
    file_paths.map { |file_path| self.new(file_path) }
  end

  # -- Instance Methods -----------------------------------------------------

  def user_params
    file_name.first.split('-')
  end

  def trial_params
    file_name.last.split('-')
  end

private

  def file_name
    @file_name ||= file_path.split('/').last.split('.txt').first.split('_')
  end

end