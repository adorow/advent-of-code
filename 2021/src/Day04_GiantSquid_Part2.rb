# --- Part Two ---
#   On the other hand, it might be wise to try a different strategy: let the giant squid win.
#
#   You aren't sure how many bingo boards a giant squid could play at once, so rather than waste time counting its arms, the safe thing to do is to figure out which board will win last and choose that one. That way, no matter which boards it picks, it will win for sure.
#
#   In the above example, the second board is the last to win, which happens after 13 is eventually called and its middle column is completely marked. If you were to keep playing until this point, the second board would have a sum of unmarked numbers equal to 148 for a final score of 148 * 13 = 1924.
#
#   Figure out which board will win last. Once it wins, what would its final score be?

# running as: `ruby ./2021/src/Day04_GiantSquid_Part2.rb`
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
winning_boards = Set.new

for number in numbers do
    used_numbers.add(number)
    boards.each_with_index do |board, index|
#     for board in boards do
        if is_complete?(board, used_numbers) then
            if not winning_boards.include?(index)  then
                position = winning_boards.size + 1
                winning_boards.add(index)
                puts "##{position}: board #{index}, after #{used_numbers.size} numbers"
                # last to win:
                if winning_boards.size == num_boards then
                    sum_unmarked = sum_unmarked(board, used_numbers)
                    last_number = number.to_i

                    # puts "Numbers: #{used_numbers}"
                    puts "Last number: #{last_number}"
                    puts "Sum unmarked: #{sum_unmarked}"
                    puts "Score: #{sum_unmarked * last_number}"
                    return
                end
            end
        end
    end

end
