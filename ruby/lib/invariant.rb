require_relative 'contract_base'

module Invariant
  include ContractBase

  def invariant(&condition_block)
    contractify_class
    add_block_to_each_call(:after, invariant_condition_block(condition_block))
  end

  private

  def invariant_condition_block(condition_block)
    proc do
      if (!instance_eval &condition_block)
        raise InvariantException
      end
    end
  end
end

class InvariantException < ContractException
end
