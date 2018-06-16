# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]], # square (only needs one)
                   rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
                   [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
                    [[0, 0], [0, -1], [0, 1], [0, 2]]],
                   rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
                   rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
                   rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
                   rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
                   [[[0, 0], [0, -1], [0, -2], [0, 1], [0, 2]], [[0, 0], [-1, 0], [-2, 0], [1, 0], [2, 0]]], #long long (only needs two)
                   rotations([[-1, -1], [0, -1], [-1, 0], [0, 0], [0, 1]]),
                   rotations([[0, 0], [0, 1], [1, 1]])]
  # your enhancements here
  def self.next_piece(myBoard)
    MyPiece.new(All_My_Pieces.sample, myBoard)
  end

  def self.cheating_piece(myBoard)
    MyPiece.new([[0, 0]], myBoard)
  end

end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @is_cheating = false
  end

  def next_piece
    if !@is_cheating
      @current_block = MyPiece.next_piece(self)
    else
      @current_block = MyPiece.cheating_piece(self)
      @is_cheating = false
    end
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.length - 1)).each {|index|
      current = locations[index];
      @grid[current[1] + displacement[1]][current[0] + displacement[0]] =
          @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def cheat
    if @score >= 100 and !@is_cheating
      @score -= 100
      @is_cheating = true
    end
  end

  def rotate_180_degrees
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

end

class MyTetris < Tetris
  # your enhancements here

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
    @root.bind('c', proc {@board.cheat})
    @root.bind('u', proc {@board.rotate_180_degrees})
  end

end