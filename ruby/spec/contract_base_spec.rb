describe 'ContractBase' do

  class ContractBaseTestClass
    attr_reader :first_before_block_calls, :first_after_block_calls
    attr_reader :second_before_block_calls, :second_after_block_calls
    attr_reader :x, :y

    def initialize
      @first_before_block_calls = 0
      @first_after_block_calls = 0

      @second_before_block_calls = 0
      @second_after_block_calls = 0

      @x = 0
      @y = 0
    end

    before_and_after_each_call(proc {@first_before_block_calls += 1}, proc {@first_after_block_calls += 1})
    before_and_after_each_call(proc {@second_before_block_calls += 1; @x = @first_before_block_calls}, proc {@second_after_block_calls += 1; @y = @first_after_block_calls})

    def sum(s1, s2)
      s1 + s2
    end

    def next(n)
      n + 1
    end

    def first_before_block_calls_equals(x)
      @first_before_block_calls == x
    end

    def first_after_block_calls_equals(x)
      @first_after_block_calls == x
    end
  end

  let(:test_object) {ContractBaseTestClass.new}


  it 'should not alter the return value of methods' do
    expect(test_object.sum(1, 5)).to eq 6
    expect(test_object.first_before_block_calls).to eq 1
    expect(test_object.first_after_block_calls).to eq 2
  end

  it 'should add "before" and "after" block to each method' do
    test_object.sum(1, 5)
    test_object.next(2)

    expect(test_object.first_before_block_calls).to eq 2
    expect(test_object.first_after_block_calls).to eq 3
  end

  it 'should call "before" block before the method call' do
    test_object.sum(2, 2)

    expect(test_object.first_before_block_calls_equals(2)).to be true
    expect(test_object.first_before_block_calls).to eq 2
  end

  it 'should call "after" block after the method call' do
    test_object.sum(2, 2)

    expect(test_object.first_after_block_calls_equals(2)).to be true
    expect(test_object.first_after_block_calls).to eq 3
  end

  it 'should add all "before" and "after" blocks to each method' do
    test_object.sum(4, 4)

    expect(test_object.first_before_block_calls).to eq 1
    expect(test_object.first_after_block_calls).to eq 2
    expect(test_object.second_before_block_calls).to eq 1
    expect(test_object.second_after_block_calls).to eq 2
  end

  it 'should call "before" and "after" blocks in order' do
    test_object.sum(4, 4)

    expect(test_object.x).to eq 1
    expect(test_object.y).to eq 2
  end
end