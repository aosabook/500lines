require 'catechism'

describe 'using catechism matchers' do
  class Harness; end

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

  it 'lets you test that an instance receives a message' do
    tacos = Harness.new
    expect { tacos.sauce }.to_send(:sauce).to(tacos)
    expect { tacos.to_s }.not.to_send(:sauce).to(tacos)
  end

  it 'lets you test that a class receives a message' do
    expect { Harness.to_s }.to_send(:to_s).to(Harness)
    expect { Harness.inspect }.not.to_send(:to_s).to(Harness)
  end

  it 'lets you test that an instance receives a message with arguments' do
    pants = Object.new
    expect {
      pants.tuck_into('socks')
    }.to_send(:tuck_into).with('socks').to(pants)

    expect {
      pants.tuck_into('ears')
    }.not.to_send(:tuck_into).with('socks').to(pants)
  end
end
