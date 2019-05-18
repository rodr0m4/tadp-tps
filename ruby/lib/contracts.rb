require_relative 'invariant'
require_relative 'condition'
require_relative 'module'

module Contracts

  attr_reader :invariants

  def invariant(&condition_block)
    contractify_class
    @invariants.push(Invariant.new(condition_block))
  end

  def pre(&block)
    contractify_class
    @next_method_precondition = Condition.new(block, :pre)
  end

  def post(&block)
    contractify_class
    @next_method_postcondition = Condition.new(block, :post)
  end

  def method_added(method_name)
    if contractified
      @method_conditions[method_name] = {:before => @next_method_precondition, :after => @next_method_postcondition}

      bound_method_to_condition(@next_method_precondition, method_name)
      bound_method_to_condition(@next_method_postcondition, method_name)

      @next_method_precondition = nil
      @next_method_postcondition = nil
    end
  end

  def bound_method_to_condition(condition, method_name)
    unless condition.nil?
      condition.my_method = instance_method method_name
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

  def method_conditions(method_name)
    @method_conditions[method_name]
  end

  def self.included(_)
    TracePoint.trace(:call) {|tp| contract_tracepoint(tp, :before)}
    TracePoint.trace(:return) {|tp| contract_tracepoint(tp, :after)}
  end

  private

  def contractify_class
    return if (contractified)
    @contractified = true

    @invariants = []
    @method_conditions = Hash.new do
      |hash, key| hash[key] = {:before => nil, :after => nil}
    end
  end

  def self.contract_tracepoint(tp, before_or_after)
    if tp.defined_class.contractified && !(before_or_after == :before && tp.callee_id == :initialize)
      tp.defined_class.invariants.each {|contract| contract.enforce(tp.self)}
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
