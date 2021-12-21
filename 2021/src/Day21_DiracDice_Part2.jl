# --- Part Two ---
#   Now that you're warmed up, it's time to play the real game.
#
#   A second compartment opens, this time labeled Dirac dice. Out of it falls a single three-sided die.
#
#   As you experiment with the die, you feel a little strange. An informational brochure in the compartment explains that this is a quantum die: when you roll it, the universe splits into multiple copies, one copy for each possible outcome of the die. In this case, rolling the die always splits the universe into three copies: one where the outcome of the roll was 1, one where it was 2, and one where it was 3.
#
#   The game is played the same as before, although to prevent things from getting too far out of hand, the game now ends when either player's score reaches at least 21.
#
#   Using the same starting positions as in the example above, player 1 wins in 444356092776315 universes, while player 2 merely wins in 341960390180808 universes.
#
#   Using your given starting positions, determine every possible outcome. Find the player that wins in more universes; in how many universes does that player win?

# run: `julia ./2021/src/Day21_DiracDice_Part1.jl`

new_position(current_pos::Int64, roll::Int64) = mod(current_pos + roll - 1, 10) + 1 #-1, +1 to account for the positions 1 through 10

input = readlines("./2021/resources/day21.in")

player1Start = parse(Int64, SubString(input[1], 29)) # skip the "Player n starting position: "
player2Start = parse(Int64, SubString(input[2], 29))

println("Player 1 starting position: ", player1Start)
println("Player 2 starting position: ", player2Start)

rollPossibilities = Dict(3 => 1, 4 => 3, 5 => 6, 6 => 7, 7 => 6, 8 => 3, 9 => 1)

wins_1 = 0
wins_2 = 0

function count_wins(turn, p1Pos, p2Pos, score1, score2, numUniverses)
    if score1 >= 21
        global wins_1 += numUniverses
    elseif score2 >= 21
        global wins_2 += numUniverses
    else
        is_p1_turn = mod(turn, 2) == 0
        for die_sum in 3:9
            newPos1 = p1Pos
            newPos2 = p2Pos
            newScore1 = score1
            newScore2 = score2
            die_occurrences = rollPossibilities[die_sum]
            if (is_p1_turn)
                newPos1 = new_position(p1Pos, die_sum)
                newScore1 += newPos1
            else
                newPos2 = new_position(p2Pos, die_sum)
                newScore2 += newPos2
            end
            count_wins(turn+1, newPos1, newPos2, newScore1, newScore2, numUniverses * die_occurrences)
        end

    end
end

start_ns = time_ns()
count_wins(0, player1Start, player2Start, 0, 0, 1)

println("Wins 1: $wins_1; Wins 2: $wins_2 => Max: $(max(wins_1, wins_2))")
end_ns = time_ns()
diff_ms = (end_ns - start_ns)/1000000
println("Execution took $(diff_ms)ms ($(diff_ms/1000)s)")
