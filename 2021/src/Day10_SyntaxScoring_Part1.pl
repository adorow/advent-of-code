#!/usr/bin/perl

#--- Day 10: Syntax Scoring ---
#You ask the submarine to determine the best route out of the deep-sea cave, but it only replies:
#
#Syntax error in navigation subsystem on line: all of them
#All of them?! The damage is worse than you thought. You bring up a copy of the navigation subsystem (your puzzle input).
#
#The navigation subsystem syntax is made of several lines containing chunks. There are one or more chunks on each line, and chunks contain zero or more other chunks. Adjacent chunks are not separated by any delimiter; if one chunk stops, the next chunk (if any) can immediately start. Every chunk must open and close with one of four legal pairs of matching characters:
#
#If a chunk opens with (, it must close with ).
#If a chunk opens with [, it must close with ].
#If a chunk opens with {, it must close with }.
#If a chunk opens with <, it must close with >.
#So, () is a legal chunk that contains no other chunks, as is []. More complex but valid chunks include ([]), {()()()}, <([{}])>, [<>({}){}[([])<>]], and even (((((((((()))))))))).
#
#Some lines are incomplete, but others are corrupted. Find and discard the corrupted lines first.
#
#A corrupted line is one where a chunk closes with the wrong character - that is, where the characters it opens and closes with do not form one of the four legal pairs listed above.
#
#Examples of corrupted chunks include (], {()()()>, (((()))}, and <([]){()}[{}]). Such a chunk can appear anywhere within a line, and its presence causes the whole line to be considered corrupted.
#
#For example, consider the following navigation subsystem:
#
#[({(<(())[]>[[{[]{<()<>>
#[(()[<>])]({[<{<<[]>>(
#{([(<{}[<>[]}>{[]{[(<()>
#(((({<>}<{<{<>}{[]{[]{}
#[[<[([]))<([[{}[[()]]]
#[{[{({}]{}}([{[{{{}}([]
#{<[[]]>}<{[{[{[]{()[[[]
#[<(<(<(<{}))><([]([]()
#<{([([[(<>()){}]>(<<{{
#<{([{{}}[<[[[<>{}]]]>[]]
#Some of the lines aren't corrupted, just incomplete; you can ignore these lines for now. The remaining five lines are corrupted:
#
#{([(<{}[<>[]}>{[]{[(<()> - Expected ], but found } instead.
#[[<[([]))<([[{}[[()]]] - Expected ], but found ) instead.
#[{[{({}]{}}([{[{{{}}([] - Expected ), but found ] instead.
#[<(<(<(<{}))><([]([]() - Expected >, but found ) instead.
#<{([([[(<>()){}]>(<<{{ - Expected ], but found > instead.
#Stop at the first incorrect closing character on each corrupted line.
#
#Did you know that syntax checkers actually have contests to see who can get the high score for syntax errors in a file? It's true! To calculate the syntax error score for a line, take the first illegal character on the line and look it up in the following table:
#
#): 3 points.
#]: 57 points.
#}: 1197 points.
#>: 25137 points.
#In the above example, an illegal ) was found twice (2*3 = 6 points), an illegal ] was found once (57 points), an illegal } was found once (1197 points), and an illegal > was found once (25137 points). So, the total syntax error score for this file is 6+57+1197+25137 = 26397 points!
#
#Find the first illegal character in each corrupted line of the navigation subsystem. What is the total syntax error score for those errors?

# run with `perl ./2021/src/Day10_SyntaxScoring_Part1.pl`

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

sub first_non_matching {
    my ($word) = @_;
    my @open_brackets = ();

    while ($word ne "") {
        my $next_char = substr($word, 0, 1);
        $word = substr($word, 1);

        print "next: $next_char\n";
        print "rest: $word\n";

        if (is_open($next_char)) {
            push @open_brackets, $next_char;
        } elsif (is_close($next_char)) {
            if (!@open_brackets) {
                return $next_char;
            }
            my $open_bracket = pop @open_brackets;
            if ($next_char ne $open_to_close{$open_bracket}) {
                return $next_char;
            }
        } else {
            # probably the end so maybe incomplete - so ignore for now
            return "";
        }
    }

    return ""; # skip
}

# reading a file / printing data
my $file = path("./2021/resources/day10.in");

my $total_points = 0;

my $file_handle = $file->openr_utf8();
while( my $line = $file_handle->getline() ) {
    print $line;
    my $mismatch = first_non_matching($line);
    given($mismatch){
         when (')') {  $total_points += 3; }
         when (']') {  $total_points += 57; }
         when ('}') {  $total_points += 1197; }
         when ('>') {  $total_points += 25137; }
         default {
             # empty, so no change
         }
    }
}

print "total points: $total_points\n";
