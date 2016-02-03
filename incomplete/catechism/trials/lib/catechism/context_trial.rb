require 'catechism'

describe 'using contexts' do
  it 'lets you group tests with describes' do
    expect(current_context.description).to_equal('using contexts')
  end
end
