# object-oriented approach for a arithmetic language
# tldr: program has a LARGE NUMBER OF DIFFERENT DATA VARIANTS (OR OBJECTS) - use OOP

# we have one big class for expressions
class Exp
  # interesting stuff herre
end

# and then, a bunch of classes that are the actual expressions inheriting from it,
# with each implementing its own rules for evaluating, being converted to string etc.
class Int < Exp
  attr_reader :i

  def initialize(i)
    @i = i
  end

  def eval
    self
  end

  def toString
    @i.to_s
  end

  def hasZero
    i == 0
  end
end

class Add < Exp
  attr_reader :e1, :e2

  def initialize(e1, e2)
    @e1 = e1
    @e2 = e2
  end

  def eval
    Int.new(e1.eval.i + e2.eval.i)
  end

  def toString
    '(' + e1.toString + ' + ' + e2.toString + ')'
  end

  def hasZero
    e1.hasZero || e2.hasZero
  end
end

# adding a new data variant is easy
class Mult < Exp
  attr_reader :e1, :e2

  def initialize(e1, e2)
    @e1 = e1
    @e2 = e2
  end

  def eval
    Int.new(e1.eval.i * e2.eval.i)
  end

  def toString
    '(' + e1.toString + ' * ' + e2.toString + ')'
  end

  def hasZero
    e1.hasZero || e2.hasZero
  end
end

# but adding a new function is hard, because we now need to go to every class and add it
