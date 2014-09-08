require 'catechism'

describe 'setting before hooks' do
  before do
    @before_block_ran = true
  end

  it 'executes before blocks before trials run' do
    expect(@before_block_ran).to_equal(true)
  end
end