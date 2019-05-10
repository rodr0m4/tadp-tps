module Contracts
  def before_and_after_each_call(before_block, after_block)
    contractify_class
    add_block_to_each_call(:before, before_block)
    add_block_to_each_call(:after, after_block)
  end

  def invariant(&condition_block)
    contractify_class
    add_block_to_each_call(:after, invariant_block(condition_block))
  end

  def pre(&block)
    set_condition(:@next_method_precondition, block)
  end

  def post(&block)
    set_condition(:@next_method_postcondition, block)
  end

  class ContractError < StandardError
  end

  class InvariantError < ContractError
  end

  class ConditionError < ContractError
  end

  def method_added(method_name)
    if (@contractified)
      @method_conditions[method_name] = {:before => @next_method_precondition, :after => @next_method_postcondition}
      @next_method_precondition = nil
      @next_method_postcondition = nil
    end
  end


  def method_has_condition?(before_or_after, method_name)
    if (@method_conditions[method_name])
      return @method_conditions[method_name][before_or_after] != nil
    end
    return false
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
    @method_conditions = {}
  end

  def contract_tracepoint(tp, before_or_after)
    if (tp.defined_class.instance_variable_get(:@contractified) && !(before_or_after == :before && tp.callee_id == :initialize))
      tp.self.class.instance_variable_get(:@each_call_blocks)[before_or_after].each {|block| tp.self.instance_exec(*tp.parameters, &block)}
      if (tp.defined_class.method_has_condition?(before_or_after, tp.callee_id))
        # tp.self.instance_exec(*tp.parameters, &tp.defined_class.instance_variable_get(:@method_conditions)[tp.callee_id][before_or_after])

        enforce_contract_with_arguments(tp, before_or_after)
      end
    end
  end

  def extract_arguments(tracepoint)
    param_names = tracepoint.parameters.map(&:last)

    param_names.inject({}) do |hash, name|
      hash[name] = tracepoint.binding.eval(name.to_s)
      hash
    end
  end

  def enforce_contract_with_arguments(tracepoint, moment)
    arguments = extract_arguments(tracepoint)
    scope = tracepoint.self.dup

    arguments.each { |name, value| scope.define_singleton_method(name) { value } }

    contract = tracepoint.defined_class.instance_variable_get(:@method_conditions)[tracepoint.callee_id][moment]

    return_value = nil

    if tracepoint.event == :return
      return_value = tracepoint.return_value
    end

    scope.instance_exec(return_value, &contract)
  end

  def set_condition(condition, block)
    contractify_class
    instance_variable_set(condition, condition_block(block))
  end

  def invariant_block(condition_block)
    proc do
      if (!instance_eval &condition_block)
        raise InvariantError
      end
    end
  end

  def condition_block(block)
    proc {|args|
      if (!instance_exec(args, &block))
        raise ConditionError
      end
    }
  end
end