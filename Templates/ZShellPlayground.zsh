#!/bin/bash
# run with: `bash ./Templates/BashPlayground.sh`

# single-line comment

# printing data
echo "Hello, World"

n=1
while read -r line; do
  echo "Line No. $n : $line"
  n=$((n+1))
done < './2021/resources/day11.in'


#Define the string value
text="Welcome to LinuxHint"
# Set space as the delimiter
IFS=' '
#Read the split words into an array based on space delimiter
read -ra strarr <<< "$text"
#Count the total words
echo "There are ${#strarr[*]} words in the text '$text'."


#Define the string to split
text="learnHTMLlearnPHPlearnMySQLlearnJavascript"
#Define multi-character delimiter
delimiter="learn"
#Concatenate the delimiter with the main string
string=$text$delimiter

#Split the text based on the delimiter
myarray=()
while [[ $string ]]; do
  myarray+=( "${string%%"$delimiter"*}" )
  string=${string#*"$delimiter"}
done

#Print the words after the split
for value in ${myarray[@]}
do
  echo -n "$value "
done
printf "\n"


a=2
echo "$a + 1"
echo "$(($a + 1))"
b=3.14
echo "$(($b + 1))" # this doesn't work in bash (works in zsh), as it only does integer arithmetic
echo "$b + 1" | bc -l # using bc (basic calculator)
#// run with: `io ./Templates/IoLanguagePlayground.io`

#arrays
# number array
allThreads=(1 2 4 8 16 32 64 128)
#mixed array
myArray=(1 2 "three" 4 "five")
echo ${allThreads[1]}
echo ${allThreads} # this actually will print only the first element
echo ${allThreads[@]} # this will print the whole thing

for t in ${allThreads[@]}; do
  echo "this array has a $t"
done

# evaluating an expression to get the result:
#runtime=$(./pipeline --threads $t)

# adding to the array
myArray+=( "newElement1" "newElement2" )

# List of logs and who should be notified of issues
logPaths=("api.log" "auth.log" "jenkins.log" "data.log")
logEmails=("jay@email" "emma@email" "jon@email" "sophia@email")

echo ${logPaths[@]} # print contents
#echo "${!logPaths[@]}" # print indexes
echo "${#logPaths[@]}" # print array size

#the ! will make it loop through indexes instead of the values
#for i in ${!logPaths[@]};
#do
#  echo $i
#  log=${logPaths[$i]}
#  stakeholder=${logEmails[$i]}
#  numErrors=$( tail -n 100 "$log" | grep "ERROR" | wc -l )
#
#  # Warn stakeholders if recently saw > 5 errors
#  if [[ "$numErrors" -gt 5 ]];
#  then
#    emailRecipient="$stakeholder"
#    emailSubject="WARNING: ${log} showing unusual levels of errors"
#    emailBody="${numErrors} errors found in log ${log}"
#    echo "$emailBody" | mailx -s "$emailSubject" "$emailRecipient"
#  fi
#done
str=$(ls) # Save ls output as a string
arr=( $(ls) )	#  ls output as an array of files
#echo "${arr[@]}"
echo "${myArray[@]:1:4}" # retrieve 2 elements starting at index 1
#echo "${arr[@]}"

#hashmap / dictionary
#the following only works in Bash 4, Bash 3 does not have support for this (only workarounds)
declare -A animals
animals=( ["moo"]="cow" ["woof"]="dog")
echo "${animals[moo]}"
for key val in "${(@kv)animals}"; do
    echo "$key -> $val"
done
#for sound in $animals; do echo "$sound - ${animals[$sound]}"; done

#conditionals
#
#
#loops
#
#
#types / structs
#
#
#functions