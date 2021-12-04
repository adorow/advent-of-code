# --- Day 4: Giant Squid ---
# You're already almost 1.5km (almost a mile) below the surface of the ocean, already so deep that you can't see any sunlight. What you can see, however, is a giant squid that has attached itself to the outside of your submarine.
#
# Maybe it wants to play bingo?
#
# Bingo is played on a set of boards each consisting of a 5x5 grid of numbers. Numbers are chosen at random, and the chosen number is marked on all boards on which it appears. (Numbers may not appear on all boards.) If all numbers in any row or any column of a board are marked, that board wins. (Diagonals don't count.)
#
# The submarine has a bingo subsystem to help passengers (currently, you and the giant squid) pass the time. It automatically generates a random order in which to draw numbers and a random set of boards (your puzzle input). For example:
#
# 7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
#
# 22 13 17 11  0
#  8  2 23  4 24
# 21  9 14 16  7
#  6 10  3 18  5
#  1 12 20 15 19
#
#  3 15  0  2 22
#  9 18 13 17  5
# 19  8  7 25 23
# 20 11 10 24  4
# 14 21 16 12  6
#
# 14 21 17 24  4
# 10 16 15  9 19
# 18  8 23 26 20
# 22 11 13  6  5
#  2  0 12  3  7
# After the first five numbers are drawn (7, 4, 9, 5, and 11), there are no winners, but the boards are marked as follows (shown here adjacent to each other to save space):
#
# 22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
#  8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
# 21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
#  6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
#  1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
# After the next six numbers are drawn (17, 23, 2, 0, 14, and 21), there are still no winners:
#
# 22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
#  8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
# 21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
#  6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
#  1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
# Finally, 24 is drawn:
#
# 22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
#  8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
# 21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
#  6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
#  1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
# At this point, the third board wins because it has at least one complete row or column of marked numbers (in this case, the entire top row is marked: 14 21 17 24 4).
#
# The score of the winning board can now be calculated. Start by finding the sum of all unmarked numbers on that board; in this case, the sum is 188. Then, multiply that sum by the number that was just called when the board won, 24, to get the final score, 188 * 24 = 4512.
#
# To guarantee victory against the giant squid, figure out which board will win first. What will your final score be if you choose that board?

# running as: `ruby ./2021/src/Day04_GiantSquid_Part1.rb`
require 'set'

file = File.open("./2021/resources/day04.in")
file_data = file.readlines.map(&:chomp)
file.close

numbers = file_data[0].split(",")
num_boards = (file_data.size - 1) / 6 # -1 to remove the header, /6 because each board starts with an empty line plus 5 lines
boards = Array.new(num_boards)

for i in 1..num_boards do
    board = Array.new(5)
    first_row = 1 + ((i-1) * 6) + 1 # 1 for header, then move the cursor to the start of this block, then skip empty line
    for j in 0..4 do
        board[j] = file_data[first_row + j].split()
    end
    boards[i-1] = board
    puts "Board #{i}: #{board}"
end

def is_complete?(board, used_numbers)
    # check rows
    for row in board do
        if row.all? { |elem| used_numbers.include?(elem) } then
            return true
        end
    end

    # check columns
    for j in 0..4 do
        all_included = true
        for i in 0..4 do
            if not used_numbers.include?(board[i][j]) then
                all_included = false
            end
        end
        if all_included then
            return true
        end
    end

    return false
end

def sum_unmarked(board, used_numbers)
    sum = 0
    for i in 0..4 do
        for j in 0..4 do
            num = board[i][j]
            if not used_numbers.include?(num) then
                sum += num.to_i
            end
        end
    end
    return sum
end

used_numbers = Set.new

for number in numbers do
    used_numbers.add(number)
    for board in boards do
        if is_complete?(board, used_numbers) then
            sum_unmarked = sum_unmarked(board, used_numbers)
            last_number = number.to_i
            puts "Winning board #{board}"
            puts "Numbers: #{used_numbers}"
            puts "Last number: #{last_number}"
            puts "Sum unmarked: #{sum_unmarked}"
            puts "Score: #{sum_unmarked * last_number}"
            return
        end
    end

end


