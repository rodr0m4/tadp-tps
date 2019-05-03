module ContractBase
  def before_and_after_each_call(before_block, after_block)
    contractify_class
    add_block_to_each_call(:before, before_block)
    add_block_to_each_call(:after, after_block)
  end

  private

  def add_block_to_each_call(before_or_after, block)
    @each_call_blocks[before_or_after].push(block)
  end

  def contractify_class
    return if (@contractified)
    @contractified = true

    TracePoint.trace(:call) {|tp| contract_tracepoint(tp, :before)}

    TracePoint.trace(:return) {|tp| contract_tracepoint(tp, :after)}

    @each_call_blocks = {:before => [], :after => []}
  end

  def contract_tracepoint(tp, before_or_after)
    if (tp.defined_class.instance_variable_get(:@contractified) && !(before_or_after == :before && tp.callee_id == :initialize))
      tp.self.class.instance_variable_get(:@each_call_blocks)[before_or_after].each {|block| tp.self.instance_exec(*tp.parameters, &block)}
    end
  end
end

class ContractException < Exception
end