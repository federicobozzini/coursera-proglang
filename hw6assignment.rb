# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  
  All_My_Pieces = All_Pieces.concat([
    rotations([[0, 0], [1, 0], [0, 1], [1, 1], [0, 2]]), # new piece num 1
    [[[0, 0], [-2, 0], [-1, 0], [1, 0], [2, 0]], # new piece num 2
     [[0, 0], [0, -2], [0, -1], [0, 1], [0, 2]]],
    rotations([[0, 0], [1, 0], [0, 1]]) # new piece num 3
  ])
  
  Cheating_Piece = [[[0,0]]]
  
  # your enhancements here
  def self.next_piece (board)
    if board.is_cheating?
      board.stop_cheating
      Piece.new(Cheating_Piece, board)
    else
      Piece.new(All_My_Pieces.sample, board)
    end
  end
end

class MyBoard < Board
  # your enhancements here
  
  @cheating = false
      
  #ugly overwrite to use MyPiece
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
  end
  
  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end  
  
  def cheat 
    if !@cheating and @score >= 100
      @score = @score - 100
      @cheating = true
    end
  end
  
  def is_cheating?
    @cheating
  end
  
  def stop_cheating
    @cheating = false
  end
    
  #ugly overwrite to use MyPiece
  def next_piece
    @current_block = MyPiece.next_piece(self)
    @current_pos = nil
  end
  
  # ugly overwrite to account for pieces with a number of block != 5
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    @current_pos.each_with_index {|pos, index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = pos
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
  
  
end

class MyTetris < Tetris
  # your enhancements here
    def key_bindings
        super
        @root.bind('u', proc {@board.rotate_180})
        @root.bind('c', proc {@board.cheat})
    end
    
    #ugly overwrite to use MyBoard
    def set_board
        @canvas = TetrisCanvas.new
        @board = MyBoard.new(self)
        @canvas.place(@board.block_size * @board.num_rows + 3,
                    @board.block_size * @board.num_columns + 6, 24, 80)
    end
end


