class Interpreter
  MAX_VALUE = 256
  def initialize(capTape = 10)
    @capTape = capTape
    @tape = [0] * @capTape
    @curProg, @curTape = 0, 0
    @debug = false
  end
  
  def run(prog)
    @prog = prog
    while @curProg < prog.size do
      step @prog[@curProg]
      puts "(#{@curProg}, #{@curTape}): " + @tape.join(' ') if @debug
    end
  end

  def step(char)
    case char
      when ?+ then @tape[@curTape] = (@tape[@curTape] + 1) % MAX_VALUE
      when ?- then @tape[@curTape]  = (@tape[@curTape] - 1) % MAX_VALUE 
      when ?> then @curTape = (@curTape + 1) % @capTape
      when ?< then @curTape = (@curTape - 1) % @capTape 
      when ?[ then goto_close if @tape[@curTape] == 0
      when ?] then goto_open
      when ?. then print @tape[@curTape].chr if @tape[@curTape] != -1
      when ?, then @tape[@curTape] = $stdin.getc.to_i || -1

      when ?# then @debug = @debug == false
    end
    @curProg += 1
  end

  def goto_close
    level = 1
    @curProg += 1
    while @curProg < @prog.size
      case @prog[@curProg]
        when ?[ then level += 1
        when ?] then level -= 1
      end
      break if level == 0
      @curProg += 1
    end
  end

  def goto_open
    level = 1
    @curProg -= 2
    while @curProg > -1
      case @prog[@curProg]
        when ?] then level += 1
        when ?[ then level -= 1 
      end
      @curProg -= 1
      break if level == 0
    end
  end
end

interpreter = Interpreter.new
interpreter.run("++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.")
