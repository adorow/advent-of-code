#!/usr/bin/perl

#--- Part Two ---
#Now, discard the corrupted lines. The remaining lines are incomplete.
#
#Incomplete lines don't have any incorrect characters - instead, they're missing some closing characters at the end of the line. To repair the navigation subsystem, you just need to figure out the sequence of closing characters that complete all open chunks in the line.
#
#You can only use closing characters (), ], }, or >), and you must add them in the correct order so that only legal pairs are formed and all chunks end up closed.
#
#In the example above, there are five incomplete lines:
#
#[({(<(())[]>[[{[]{<()<>> - Complete by adding }}]])})].
#[(()[<>])]({[<{<<[]>>( - Complete by adding )}>]}).
#(((({<>}<{<{<>}{[]{[]{} - Complete by adding }}>}>)))).
#{<[[]]>}<{[{[{[]{()[[[] - Complete by adding ]]}}]}]}>.
#<{([{{}}[<[[[<>{}]]]>[]] - Complete by adding ])}>.
#Did you know that autocomplete tools also have contests? It's true! The score is determined by considering the completion string character-by-character. Start with a total score of 0. Then, for each character, multiply the total score by 5 and then increase the total score by the point value given for the character in the following table:
#
#): 1 point.
#]: 2 points.
#}: 3 points.
#>: 4 points.
#So, the last completion string above - ])}> - would be scored as follows:
#
#Start with a total score of 0.
#Multiply the total score by 5 to get 0, then add the value of ] (2) to get a new total score of 2.
#Multiply the total score by 5 to get 10, then add the value of ) (1) to get a new total score of 11.
#Multiply the total score by 5 to get 55, then add the value of } (3) to get a new total score of 58.
#Multiply the total score by 5 to get 290, then add the value of > (4) to get a new total score of 294.
#The five lines' completion strings have total scores as follows:
#
#}}]])})] - 288957 total points.
#)}>]}) - 5566 total points.
#}}>}>)))) - 1480781 total points.
#]]}}]}]}> - 995444 total points.
#])}> - 294 total points.
#Autocomplete tools are an odd bunch: the winner is found by sorting all of the scores and then taking the middle score. (There will always be an odd number of scores to consider.) In this example, the middle score is 288957 because there are the same number of scores smaller and larger than it.
#
#Find the completion string for each incomplete line, score the completion strings, and sort the scores. What is the middle score?

# run with `perl ./2021/src/Day10_SyntaxScoring_Part2.pl`

use strict;
use warnings;

use Path::Tiny; # Path::Tiny makes working with directories and files clean and easy to do. Use path() to create a Path::Tiny object
# but remember if you are calling other Perl modules you may need to convert the object to a string using 'stringify': $file->stringify();
use autodie; # die if problem reading or writing a file
use feature "switch"; # to enable 'given'


my %open_to_close =  ( '(' => ')', '[' => ']', '{' => '}', '<' => '>');

sub is_open {
    my ($char) = @_;
    return  grep { $char eq $_ } (keys %open_to_close);
}

sub is_close {
    my ($char) = @_;
    return  grep { $char eq $_ } (values %open_to_close);
}

sub  trim {
    my $s = shift;
    $s =~ s/^\s+|\s+$//g;
    return $s;
}

sub find_missing_ending {
    my ($word) = @_;
    my @open_brackets = ();

    while ($word ne "") {
        my $next_char = substr($word, 0, 1);
        $word = substr($word, 1);

        #print "next: $next_char\n";
        #print "rest: $word\n";

        if (is_open($next_char)) {
            push @open_brackets, $next_char;
        } elsif (is_close($next_char)) {
            # if there's a mismatch, return empty to ignore the result
            if (!@open_brackets) {
                return 0;
            }
            my $open_bracket = pop @open_brackets;
            # if there's a mismatch, return empty to ignore the result
            if ($next_char ne $open_to_close{$open_bracket}) {
                return 0;
            }
        } else {
            die "shouldn't get here: '$next_char'";
        }
    }

    print "Word ended, open brackets left: @open_brackets\n";

    my $total_points = 0;
    while (@open_brackets) {
        $total_points *= 5;
        my $open_missing_closed = pop @open_brackets;
        given($open_missing_closed){
             when ('(') {  $total_points += 1; }
             when ('[') {  $total_points += 2; }
             when ('{') {  $total_points += 3; }
             when ('<') {  $total_points += 4; }
             default {
                 # empty, so no change
             }
        }
    }

    return $total_points;
}

# reading a file / printing data
my $file = path("./2021/resources/day10.in");

my @all_points = ();

my $file_handle = $file->openr_utf8();
while( my $line = trim($file_handle->getline()) ) {
    print "$line\n";

    my $total_points = find_missing_ending($line);
    print "total points: $total_points\n";
    push @all_points, $total_points if $total_points > 0;
}

my $all_points_length = @all_points;
my @sorted_points = sort @all_points;
my $middle_score = $sorted_points[$all_points_length / 2];

print "middle score: $middle_score\n";
