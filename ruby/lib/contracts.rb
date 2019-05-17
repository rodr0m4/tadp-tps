require_relative 'each_call_block'
require_relative 'invariant'
require_relative 'condition'

module Contracts
  def before_and_after_each_call(before_block, after_block)
    contractify_class
    add_block_to_each_call(:before, before_block)
    add_block_to_each_call(:after, after_block)
  end

  def invariant(&condition_block)
    contractify_class
    @invariants.push(Invariant.new(condition_block))
  end

  def pre(&block)
    contractify_class
    instance_variable_set(:@next_method_precondition, Condition.new(block, :pre))
  end

  def post(&block)
    contractify_class
    instance_variable_set(:@next_method_postcondition, Condition.new(block, :post))
  end

  def method_added(method_name)
    if @contractified
      @method_conditions[method_name] = {:before => @next_method_precondition, :after => @next_method_postcondition}
      @next_method_precondition = nil
      @next_method_postcondition = nil
    end
  end

  def class_contracts(before_or_after)
    case before_or_after
    when :before
      return @each_call_blocks[:before]
    when :after
      return @invariants + @each_call_blocks[:after]
    else
      raise ArgumentError
    end
  end

  def method_has_condition?(method_name, before_or_after)
    if @method_conditions[method_name] && @method_conditions[method_name][before_or_after]
      return true
    end
    false
  end

  def method_condition(method_name, before_or_after)
    @method_conditions[method_name][before_or_after]
  end

  def self.included(_)
    TracePoint.trace(:call) {|tp| contract_tracepoint(tp, :before)}
    TracePoint.trace(:return) {|tp| contract_tracepoint(tp, :after)}
  end

  private

  def add_block_to_each_call(before_or_after, block)
    @each_call_blocks[before_or_after].push(EachCallBlock.new(block))
  end

  def contractify_class
    return if (@contractified)
    @contractified = true

    @each_call_blocks = {:before => [], :after => []}
    @invariants = []
    @method_conditions = {}
  end

  def self.contract_tracepoint(tp, before_or_after)
    if tp.defined_class.instance_variable_get(:@contractified) && !(before_or_after == :before && tp.callee_id == :initialize)
      tp.defined_class.class_contracts(before_or_after).each {|contract| contract.enforce(tp.self)}
      if tp.defined_class.method_has_condition?(tp.callee_id, before_or_after)
        enforce_condition_with_arguments(tp, before_or_after)
      end
    end
  end

  def self.extract_arguments(tracepoint)
    param_names = tracepoint.parameters.map(&:last)

    param_names.inject({}) do |hash, name|
      hash[name] = tracepoint.binding.eval(name.to_s)
      hash
    end
  end

  def self.enforce_condition_with_arguments(tracepoint, before_or_after)
    arguments = extract_arguments(tracepoint)
    context = tracepoint.self.dup

    arguments.each {|name, value| context.define_singleton_method(name) {value}}

    contract = tracepoint.defined_class.method_condition(tracepoint.callee_id, before_or_after)

    return_value = nil

    if tracepoint.event == :return
      return_value = tracepoint.return_value
    end

    contract.enforce(context, return_value)
  end
end
