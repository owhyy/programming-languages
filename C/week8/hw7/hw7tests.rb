require_relative('./hw7')

ZERO = 0.0
ONE = 1.0
TWO = 2.0
THREE = 3.0
FOUR = 4.0
FIVE = 5.0
SIX = 6.0
SEVEN = 7.0
TEN = 10.0

# Var Tests
v = Var.new('a')
v1 = v.eval_prog([['a', Point.new(THREE, FIVE)]])
puts 'Var eval_prog is not working properly' unless (v1.is_a? Point) and v1.x == THREE and v1.y == FIVE
puts 'Var preprocess_prog should return self' unless v.preprocess_prog == v

# Let Tests
l = Let.new('a', LineSegment.new(-ONE, -TWO, THREE, FOUR),
            Intersect.new(Var.new('a'), LineSegment.new(THREE, FOUR, -ONE, -TWO)))
l1 = l.preprocess_prog.eval_prog([])
unless l1.x1 == -ONE and l1.y1 == -TWO and l1.x2 == THREE and l1.y2 == FOUR
  puts 'Let eval_prog should evaluate e2 after adding [s, e1] to the environment'
end
