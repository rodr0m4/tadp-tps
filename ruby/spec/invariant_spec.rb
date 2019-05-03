describe 'Invariant' do

  class InvariantTestClass
    attr_reader :total

    def initialize(total)
      @total = total
    end

    invariant {@total <= 20}
    invariant {@total >= 5}

    def add(x)
      @total += x
    end
  end

  let(:test_object) {InvariantTestClass.new(5)}


  it 'should let the program continue if the conditions are met' do
    test_object.add(5)
    expect(test_object.total).to eq 10
  end

  it 'should raise an exception if any condition is not met after a method call' do
    expect {test_object.add(20)}.to raise_exception(InvariantException)
  end

  it 'should raise an exception if any condition is not met after a method call' do
    expect {test_object.add(-10)}.to raise_exception(InvariantException)
  end

  it 'should raise an exception if any condition is not met after object initialization' do
    expect {InvariantTestClass.new(21)}.to raise_exception(InvariantException)
  end
end