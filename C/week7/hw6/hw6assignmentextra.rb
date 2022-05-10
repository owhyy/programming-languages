# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the "challange" problem. Although there isn't one, I've decided to invent my own, so
# I'll be implemeneting:
# 1. A "Game Over" menu
# 2. A difficulty system, and a menu for chosing it

class MyPieceExtra < MyPiece
end

class MyBoardExtra < MyBoard
  def initialize(game)
    super
    @delay_per_tick = 2
  end

  attr_writer :delay, :delay_per_tick
end

class MyTetrisExtra < MyTetris
  def initialize
    @root = TetrisRoot.new
    @difficulty = :easy
    draw_difficulty_menu
    @timer = TetrisTimer.new
    @running = true
  end

  def draw_difficulty_menu
    background = TetrisCanvas.new
    background.place(615, 215, 0, 0)
    difficulty_label = TetrisLabel.new(@root) do
      text 'Choose the difficulty'
      background 'lightblue'
    end
    difficulty_label.place(20, 150, 30, 150)

    easy = TetrisButton.new('Easy', 'lightgreen') do
      start_game :easy
    end
    easy.place(30, 80, 65, 200)

    medium = TetrisButton.new('Medium', 'yellow') do
      start_game :medium
    end
    medium.place(30, 80, 65, 280)

    hard = TetrisButton.new('Hard', 'lightcoral') do
      start_game :hard
    end
    hard.place(30, 80, 65, 360)

    quit = TetrisButton.new('Quit', 'lightblue') { exitProgram }
    quit.place(30, 80, 65, 500)
  end

  def start_game(difficulty)
    @root.restart

    @difficulty = difficulty
    set_board
    key_bindings
    buttons
    run_game
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoardExtra.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    case @difficulty
    when :easy
      @board.delay_per_tick = 2
      @board.delay = 300
    when :medium
      @board.delay_per_tick = 4
      @board.delay = 250
    when :hard
      @board.delay_per_tick = 8
      @board.delay = 150
    end

    @board.draw
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
    @delay = [@delay - delay_per_tick, 80].max # increase the speed
    puts @board.delay
  end

  def game_over_screen
    @root.restart
    background = TetrisCanvas.new
    background.place(615, 215, 0, 0)
    game_over_label = TetrisLabel.new(@root) do
      text 'Game Over'
      background 'gray'
    end
    game_over_label.place(30, 150, 30, 100)

    game_score = @board.score
    score_label = TetrisLabel.new(@root) do
      text "Current score: #{game_score}"
      background 'gray'
    end
    score_label.place(30, 150, 30, 150)

    play_again = TetrisButton.new('Play Again', 'lightblue') do
      draw_difficulty_menu
    end

    play_again.place(30, 80, 65, 200)

    quit = TetrisButton.new('Quit', 'lightblue') do
      exitProgram
    end

    quit.place(30, 80, 65, 250)
  end

  def run_game
    game_over_screen if @board.game_over?
    return unless !@board.game_over? && @running

    @timer.stop
    @timer.start(@board.delay, (proc {
                                  @board.run
                                  run_game
                                }))
  end
end
