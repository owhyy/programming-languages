#  University of Washington, Programming Languages, Homework 6, hw6runner.rb

require_relative './hw6provided'
require_relative './hw6assignment'
require_relative './hw6assignmentextra'

def runTetris
  Tetris.new # initilalizes new tetris object
  mainLoop   # calls the mainLoop
end

def runMyTetris
  MyTetris.new
  mainLoop
end

def runMyTetrisExtra
  MyTetrisExtra.new
  mainLoop
end

if ARGV.count == 0
  runMyTetris
elsif ARGV.count != 1
  puts "usage: hw6runner.rb [enhanced | original | extra]"
elsif ARGV[0] == "enhanced"
  runMyTetris
elsif ARGV[0] == "extra"
  runMyTetrisExtra
elsif ARGV[0] == "original"
  runTetris
else
  puts "usage: hw6runner.rb [enhanced | original | extra]"
end
