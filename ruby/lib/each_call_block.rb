class EachCallBlock
  def initialize(block)
    @block = block
  end

  def enforce(instance)
    instance.instance_exec(&@block)
  end
end
