# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = All_Pieces.push(
    rotations([[0, 0], [0, 1], [1, 0], [1, 1], [1, 2]]), # square with 1 right cube
    [[[0, 0], [-1, 0], [1, 0], [2, 0], [3, 0]],          # 5-length long
     [[0, 0], [0, -1], [0, 1], [0, 2], [0, 3]]],
    rotations([[0, 0], [0, -1], [1, 0]]) # rotating when in L position does not work for some reason
  )
end

class MyBoard < Board
  def initialize(game)
    super
    @cheat = false
  end

  def cheat?
    @cheat
  end

  def rotate180
    @current_block.move(0, 0, 2) if !game_over? && @game.is_running?
    draw
  end

  def store_current
    locations = @current_block.current_rotation # positions of the object
    displacement = @current_block.position      # actual location of the [0, 0] block of the positions
    n_of_positions = @current_block.current_rotation.size - 1 # so objects that have 3 or 5 coordinates work
    (0..n_of_positions).each do |index|
      current = locations[index]; # current rotation
      @grid[current[1] + displacement[1]][current[0] + displacement[0]] = # set the object on the grid
        @current_pos[index]
    end
    remove_filled
    @delay = [@delay - 2, 80].max # increase the speed
  end

  def next_piece
    if cheat?
      @current_block = Piece.new([[[0, 0]]], self)
      @cheat = false
    else
      @current_block = Piece.next_piece(self)
    end
    @current_pos = nil
  end

  def cheat
    if @game.is_running? && !game_over? && (@score >= 100) && !cheat?
      @cheat = true
      @score -= 100
    end
    draw
  end
end

class MyTetris < Tetris
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('u', proc { @board.rotate180 })
    @root.bind('c', proc { @board.cheat })
  end
end
