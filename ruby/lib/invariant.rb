require_relative 'contract_error'
require_relative 'contracts'

class Invariant
  def initialize(block)
    @block = block
  end

  def enforce(instance)
    if !instance.instance_exec(&@block)
      raise InvariantError
    end
  end
end

class InvariantError < ContractError
end
