require 'catechism'

describe 'basic catechism matchers' do
  it 'lets you test equality' do
    expect(1).to_equal(1)
    expect(2).not.to_equal(1)
  end

  it 'lets you test nilness' do
    expect(nil).to_be_nil
    expect(1).not.to_be_nil
  end

  it 'lets you test exception raising' do
    expect { raise 'hands' }.to_raise_error
    expect { 3 + 1 }.not.to_raise_error
  end

  it 'lets you test numeric changes' do
    fish = 1
    expect { fish += 1 }.to_change { fish }.by(1)
  end
end
