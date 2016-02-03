require 'catechism'

describe 'setting after hooks' do
  after do
    expect(@main_body_finished).to_equal(true)
  end

  it 'executes after the trial' do
    expect(current_context.after_blocks.count).to_equal(1)
    @main_body_finished = true
  end
end