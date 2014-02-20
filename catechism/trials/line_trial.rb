require 'catechism'

describe 'the number of lines' do
  it 'stays under 500' do
    lib_files = Dir.glob(File.expand_path('../../lib/**/*.rb', __FILE__))
    bin_files = Dir.glob(File.expand_path('../../bin/*', __FILE__))
    trial_files = Dir.glob(File.expand_path('../../trials/**/*.rb', __FILE__))

    lines = (lib_files + bin_files + trial_files).reduce(0) do |line_count, filename|
      line_count + IO.readlines(filename).size
    end

    expect(lines < 500).to_equal(true)
    puts "Current number of lines is #{lines}"
  end
end
