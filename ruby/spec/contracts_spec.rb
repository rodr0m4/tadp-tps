# TODO Test before_and_after_each_call

class Subject
  attr_reader :witness

  def initialize(witness)
    @witness = witness
  end
end

describe Contracts do
  before :each do
    @witness = double()
    allow(@witness).to receive(:notify)
  end

  describe "#pre and #post" do
    describe "#pre" do
      it "should not call the method if the contract fails" do
        class FailsPreM < Subject
          pre do
            false
          end

          def m
            witness.notify
          end
        end

        subject = FailsPreM.new(@witness)
        expect(@witness).to_not receive(:notify)

        expect {subject.m}.to raise_error(ContractError)
      end

      it "should call the method if the contract passes" do
        class PassesPreM < Subject
          pre do
            true
          end

          def m
            witness.notify
          end
        end

        subject = PassesPreM.new(@witness)
        expect(@witness).to receive(:notify)

        expect {subject.m}.to_not raise_error
      end

      it "should have the methods/instance variables from the object in context" do
        class ContextSubject < Subject
          attr_reader :passes

          def initialize(witness, passes)
            super(witness)
            @passes = passes
          end

          pre {passes}

          def m
            witness.notify
          end
        end

        expect(@witness).to receive(:notify).once

        should_pass = ContextSubject.new(@witness, true)
        expect {should_pass.m}.to_not raise_error

        should_not_pass = ContextSubject.new(@witness, false)
        expect {should_not_pass.m}.to raise_error(ContractError)
      end

      it "should have the parameters of the method in context" do
        class ParametersInContext < Subject
          pre {passes}

          def m(passes)
            witness.notify
          end
        end

        parameters_in_context = ParametersInContext.new(@witness)

        expect(@witness).to receive(:notify).once
        expect {parameters_in_context.m(true)}.to_not raise_error

        expect {parameters_in_context.m(false)}.to raise_error(ContractError)
      end

      it "should work with super" do
        class Parent
          def m(foo)
          end
        end

        class Child < Parent
          post {@bar}

          def m(foo)
            super(foo)
            @bar = true
          end
        end

        expect {
          should_pass = Child.new
          should_pass.m(true)
        }.to_not raise_error
      end
    end

    describe "#post" do
      it "should fail after the method is called" do
        class FailAfterMethodCall < Subject
          post {false}

          def m
            witness.notify
          end
        end

        subject = FailAfterMethodCall.new(@witness)
        expect(@witness).to receive(:notify).once

        expect {subject.m}.to raise_error(ContractError)
      end

      it "should take the method result as a parameter" do
        class WithResultInConsideration
          post {|result| result}

          def m(passes)
            passes
          end
        end

        subject = WithResultInConsideration.new
        expect {subject.m(true)}.to_not raise_error

        expect {subject.m(false)}.to raise_error(ContractError)
      end
    end

    it "should not leak contracts over other methods" do
      class NoLeak
        pre {false}
        post {false}

        def will_not_pass
        end

        def will_pass
        end
      end

      subject = NoLeak.new
      expect {subject.will_pass}.to_not raise_error

      expect {subject.will_not_pass}.to raise_error(ContractError)
    end
  end

  describe "#invariant" do
    it "should be called after every method called" do
      class WithInvariant < Subject
        attr_reader :passes

        invariant {true}

        def m
        end
      end

      expect do
        should_pass = WithInvariant.new(@witness)
        should_pass.m
      end.to_not raise_error
    end

    it "should have instance variables in context" do
      class WithInstanceVariable < Subject
        invariant {@passes}

        def initialize(witness, passes)
          @witness = witness
          @passes = passes
        end

        def m
          witness.notify
        end
      end

      expect(@witness).to receive(:notify).once

      expect do
        should_pass = WithInstanceVariable.new(@witness, true)
        should_pass.m
      end.to_not raise_error

      expect do
        should_not_pass = WithInstanceVariable.new(@witness, false)
        should_not_pass.m
      end.to raise_error(InvariantError)
    end

    it "should have instance methods in context" do
      class WithMethod < Subject
        attr_reader :passes
        invariant {passes}

        def initialize(witness, passes)
          @witness = witness
          @passes = passes
        end

        def m
          witness.notify
        end
      end

      expect(@witness).to receive(:notify).once

      expect do
        should_pass = WithMethod.new(@witness, true)
        should_pass.m
      end.to_not raise_error

      expect do
        should_not_pass = WithMethod.new(@witness, false)
        should_not_pass.m
      end.to raise_error(InvariantError)
    end

    it "should work with super" do
      class WithSuperUsage < Subject
        invariant {@foo}

        def initialize(witness, foo)
          super(witness)
          @foo = foo
        end
      end

      expect {
        should_pass = WithSuperUsage.new(@witness, true)
      }.to_not raise_error

      expect {WithSuperUsage.new(@witness, false)}.to raise_error(InvariantError)
    end
  end
end