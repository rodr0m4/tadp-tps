require_relative 'contract_error'

class Condition
  def initialize(block, pre_or_post)
    @block = block
    @pre_or_post = pre_or_post
  end

  def enforce(context, return_value)
    if !context.instance_exec(return_value, &@block)
      raise ConditionError
    end
  end
end

# TODO Show a descriptive message depending on the type of condition (pre or post)
class ConditionError < ContractError
end
