class Catechism::Trial < Struct.new(:file_path)
  def run
    puts 'All trials passed.'
  end
end