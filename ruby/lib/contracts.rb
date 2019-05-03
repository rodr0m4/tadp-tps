require_relative 'contract_base'
require_relative 'invariant'

module Contracts
  include ContractBase
  include Invariant
end