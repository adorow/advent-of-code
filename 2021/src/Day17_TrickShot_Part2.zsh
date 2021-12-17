#!/bin/zsh
#--- Part Two ---
#Maybe a fancy trick shot isn't the best idea; after all, you only have one probe, so you had better not miss.
#
#To get the best idea of what your options are for launching the probe, you need to find every initial velocity that causes the probe to eventually be within the target area after any step.
#
#In the above example, there are 112 different initial velocity values that meet these criteria:
#
#23,-10  25,-9   27,-5   29,-6   22,-6   21,-7   9,0     27,-7   24,-5
#25,-7   26,-6   25,-5   6,8     11,-2   20,-5   29,-10  6,3     28,-7
#8,0     30,-6   29,-8   20,-10  6,7     6,4     6,1     14,-4   21,-6
#26,-10  7,-1    7,7     8,-1    21,-9   6,2     20,-7   30,-10  14,-3
#20,-8   13,-2   7,3     28,-8   29,-9   15,-3   22,-5   26,-8   25,-8
#25,-6   15,-4   9,-2    15,-2   12,-2   28,-9   12,-3   24,-6   23,-7
#25,-10  7,8     11,-3   26,-7   7,1     23,-9   6,0     22,-10  27,-6
#8,1     22,-8   13,-4   7,6     28,-6   11,-4   12,-4   26,-9   7,4
#24,-10  23,-8   30,-8   7,0     9,-1    10,-1   26,-5   22,-9   6,5
#7,5     23,-6   28,-10  10,-2   11,-1   20,-9   14,-2   29,-7   13,-3
#23,-5   24,-8   27,-9   30,-7   28,-5   21,-10  7,9     6,6     21,-5
#27,-10  7,2     30,-9   21,-8   22,-7   24,-9   20,-6   6,9     29,-5
#8,-2    27,-8   30,-5   24,-7
#How many distinct initial velocity values cause the probe to be within the target area after any step?
# run with: `zsh ./2021/src/Day17_trickShot_Part2.zsh`

#done < './2021/resources/day17.in'

# The version that didn't use 'declare -i' to declare integers, was running in 135 seconds (2:15 minutes)
# The current version runs in 2.5 seconds

declare -i target_x_min
declare -i target_x_max
declare -i target_y_min
declare -i target_y_max

read -r line < './2021/resources/day17.in' # read a single line
echo "Line: $line"
if [[ "$line" =~ "x=([0-9]+)\.\.([0-9]+), y=(-?[0-9]+)\.\.(-?[0-9]+)" ]]; then
  target_x_min=${match[1]}
  target_x_max=${match[2]}
  target_y_min=${match[3]}
  target_y_max=${match[4]}
fi

declare -i n_solutions=0

declare -i max_y_velocity=-target_y_min-1
declare -i min_y_velocity=target_y_min

zmodload zsh/mathfunc # load module to run 'sqrt'
declare -i min_x_velocity=$(( sqrt(1 + 8 * target_x_min) / 2 ))

declare -i max_x_velocity=target_x_max

function will_reach_target_in_any_step {
  declare -i x_initial_velocity=$1
  declare -i y_initial_velocity=$2
  #declare -i iteration=0
  declare -i x=$x_initial_velocity
  declare -i y=$y_initial_velocity
  declare -i current_x_velocity=$x_initial_velocity
  declare -i current_y_velocity=$y_initial_velocity

  while (( (x <= max_x_velocity) && (y >= min_y_velocity))); do
    #iteration+=1
    #echo "iter $iteration: ($x, $y), velocities: ($current_x_velocity, $current_y_velocity)"
    if (( x >= target_x_min && x <= target_x_max && y >= target_y_min && y <= target_y_max )); then
      #echo "> ($x_velocity, $y_velocity) velocity reached ($x, $y) on $iteration iterations"
      # 0 = true
      return 0
    fi
    if (( current_x_velocity > 0 )); then
        (( current_x_velocity=current_x_velocity-1 ))
    fi
    (( current_y_velocity=current_y_velocity-1 ))
    x+=current_x_velocity
    y+=current_y_velocity
  done
  # 1 = false
  return 1
}

declare -i y_velocity=$min_y_velocity
while (( y_velocity <= max_y_velocity)); do
  # echo "verifying $y_velocity of $max_y_velocity"
  declare -i x_velocity=$min_x_velocity

  while (( x_velocity <= max_x_velocity )); do
    # echo "verifying velocities ($x_velocity, $y_velocity)"
    if will_reach_target_in_any_step $x_velocity $y_velocity; then
      # echo "($x_velocity, $y_velocity)"
      n_solutions=$((n_solutions + 1))
    fi

    x_velocity=$((x_velocity + 1))
  done
  y_velocity=$((y_velocity + 1))
done

echo "Total solutions: $n_solutions"
