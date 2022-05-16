# double dispatch implementation of binary operations (operations on more thatn 1 type) for our arithmetic programming language is a bit harder to understand, but more beautiful that the functional approach

# there are really 2 approaches: first, using the same logic as in the functional approach, where we have functions defined for every type, and addition is performed via calling a intermediare add_values function. this will force use to implement one such function for every type:
class Int
  def eval
    e1.eval.add_values e2.eval
    #            ^      we'll now implement add_values in every type
  end
  def add_values v
    if v.is_a? Int
      ...
    elsif v.is_a? MyRational
      ...
    else
      ...
    end
  end
end

# same thing for MyRational...
class MyRational
  def add_values v
    if v.is_a? Int
      ...
    elsif
      ...
    else
      ...
    end
  end
end

# however, there's a second approach, using Double Dispatch
# the problem is that we need to know the class of the argument v; in OOP we replace this "needing to know" with calling a mehod on v, which will automatically behave correctly, in dependence of v's type

class Int
  def add_values v # this is the so-called first-dispatch: when called on Int, we alreay know that the type is Int
    v.addInt self
  end
  def addInt v
    Int.new(v.i + i)
  end
  def addString v
    MyString.new(v.s, i.to_s)
  end
  def addRational v
    MyRational.new(v.i + v.j*i, v.j)
  end
end

# ...
