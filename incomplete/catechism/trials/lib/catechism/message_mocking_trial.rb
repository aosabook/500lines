require 'catechism'

describe 'using the mocking matchers' do
  class Harness; end

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

  it 'unmocks the message at the end of the test' do
    pants = Object.new
    expect { pants.tuck_into('socks') }.to_raise_error(NoMethodError)
  end
end
