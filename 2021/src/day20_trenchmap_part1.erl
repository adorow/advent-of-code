%%
%--- Day 20: Trench Map ---
%With the scanners fully deployed, you turn their attention to mapping the floor of the ocean trench.
%
%When you get back the image from the scanners, it seems to just be random noise. Perhaps you can combine an image enhancement algorithm and the input image (your puzzle input) to clean it up a little.
%
%For example:
%
%..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##
%#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###
%.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.
%.#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....
%.#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..
%...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....
%..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#
%
%#..#.
%#....
%##..#
%..#..
%..###
%The first section is the image enhancement algorithm. It is normally given on a single line, but it has been wrapped to multiple lines in this example for legibility. The second section is the input image, a two-dimensional grid of light pixels (#) and dark pixels (.).
%
%The image enhancement algorithm describes how to enhance an image by simultaneously converting all pixels in the input image into an output image. Each pixel of the output image is determined by looking at a 3x3 square of pixels centered on the corresponding input image pixel. So, to determine the value of the pixel at (5,10) in the output image, nine pixels from the input image need to be considered: (4,9), (4,10), (4,11), (5,9), (5,10), (5,11), (6,9), (6,10), and (6,11). These nine input pixels are combined into a single binary number that is used as an index in the image enhancement algorithm string.
%
%For example, to determine the output pixel that corresponds to the very middle pixel of the input image, the nine pixels marked by [...] would need to be considered:
%
%# . . # .
%#[. . .].
%#[# . .]#
%.[. # .].
%. . # # #
%Starting from the top-left and reading across each row, these pixels are ..., then #.., then .#.; combining these forms ...#...#.. By turning dark pixels (.) into 0 and light pixels (#) into 1, the binary number 000100010 can be formed, which is 34 in decimal.
%
%The image enhancement algorithm string is exactly 512 characters long, enough to match every possible 9-bit binary number. The first few characters of the string (numbered starting from zero) are as follows:
%
%0         10        20        30  34    40        50        60        70
%|         |         |         |   |     |         |         |         |
%..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##
%In the middle of this first group of characters, the character at index 34 can be found: #. So, the output pixel in the center of the output image should be #, a light pixel.
%
%This process can then be repeated to calculate every pixel of the output image.
%
%Through advances in imaging technology, the images being operated on here are infinite in size. Every pixel of the infinite output image needs to be calculated exactly based on the relevant pixels of the input image. The small input image you have is only a small region of the actual infinite input image; the rest of the input image consists of dark pixels (.). For the purposes of the example, to save on space, only a portion of the infinite-sized input and output images will be shown.
%
%The starting input image, therefore, looks something like this, with more dark pixels (.) extending forever in every direction not shown here:
%
%...............
%...............
%...............
%...............
%...............
%.....#..#......
%.....#.........
%.....##..#.....
%.......#.......
%.......###.....
%...............
%...............
%...............
%...............
%...............
%By applying the image enhancement algorithm to every pixel simultaneously, the following output image can be obtained:
%
%...............
%...............
%...............
%...............
%.....##.##.....
%....#..#.#.....
%....##.#..#....
%....####..#....
%.....#..##.....
%......##..#....
%.......#.#.....
%...............
%...............
%...............
%...............
%Through further advances in imaging technology, the above output image can also be used as an input image! This allows it to be enhanced a second time:
%
%...............
%...............
%...............
%..........#....
%....#..#.#.....
%...#.#...###...
%...#...##.#....
%...#.....#.#...
%....#.#####....
%.....#.#####...
%......##.##....
%.......###.....
%...............
%...............
%...............
%Truly incredible - now the small details are really starting to come through. After enhancing the original input image twice, 35 pixels are lit.
%
%Start with the original input image and apply the image enhancement algorithm twice, being careful to account for the infinite size of the images. How many pixels are lit in the resulting image?

%% run with: `echo "cd('./2021/src/'). c(day20_trenchmap_part1). day20_trenchmap_part1:main(). init:stop()." | erl`

%% had to name this file like this because of Erlang's module naming
-module(day20_trenchmap_part1).
-compile([export_all]). %% can export all the functions

print(What) -> io:format("~p", [What]).
println(What) -> io:format("~p~n", [What]).

-define(DARK, 46). %% .
-define(LIGHT, 35). %% #

main() ->
  { Enhancement, Image } = readInput(),
  Enhanced = enhance_n_times(2, Image, Enhancement),

  print_image("Enhanced image", Enhanced),
  println("Lit pixels: " ++ integer_to_list(count_light_pixels(Enhanced))),
  ok.

%% enhance _n_ times, where we add borders _n_ times, and then enhance _n_ times
enhance_n_times(N, Image, Enhancement) ->
  N_Extra_Borders = N,
  WithBorder = with_n_borders(N + N_Extra_Borders, Image),
  enhance_n_times_(N, WithBorder, Enhancement).

enhance_n_times_(0, Image, _) ->
  Image;
enhance_n_times_(N, Image, Enhancement) ->
  enhance_n_times_(N-1, enhance(Image, Enhancement), Enhancement).

with_n_borders(0, Image) ->
  Image;
with_n_borders(N, Image) ->
  with_n_borders(N-1, with_border(Image)).

enhance(Image, _) when length(Image) < 3 ->
  [];
enhance(Image, Enhancement) ->
  [FirstRow | [SecondRow | [ThirdRow | _]]] = Image,
  EnhancedRow = enhance_row(FirstRow, SecondRow, ThirdRow, Enhancement),
  [ EnhancedRow | enhance(tail(Image), Enhancement)].
enhance_row(FirstRow, SecondRow, ThirdRow, Enhancement) when length(FirstRow) < 3 ->
  "";
enhance_row(FirstRow, SecondRow, ThirdRow, Enhancement) ->
  Index = extract_index(FirstRow, SecondRow, ThirdRow),
  NewValue = nth_element(Index, Enhancement),
  [ NewValue | enhance_row(tail(FirstRow), tail(SecondRow), tail(ThirdRow), Enhancement)].

tail([_|T]) -> T.

nth_element(Index, List) ->
  nth_element_(Index, List, 0).

nth_element_(Index, [H|_], CurIndex) when Index == CurIndex->
  H;
nth_element_(Index, [_|T], CurIndex) ->
  nth_element_(Index, T, CurIndex+1).

count_light_pixels(Image) ->
  length([ 1 || Row <- Image, Cell <- Row, Cell == ?LIGHT ]).

extract_index(FirstRow, SecondRow, ThirdRow) ->
  DarkLightStr = string:left(FirstRow, 3) ++ string:left(SecondRow, 3) ++ string:left(ThirdRow, 3),
  BinStr = dark_and_light_to_bin_string(DarkLightStr),
  bin_string_to_int(BinStr).

bin_string_to_int(Bin) ->
  bin_string_to_int_(Bin, 0).

bin_string_to_int_("", Acc) ->
  Acc;
bin_string_to_int_([H|RemBin], Acc) ->
  bin_string_to_int_(RemBin, (Acc * 2) + list_to_integer([H])).

dark_and_light_to_bin_string([]) ->
  "";
dark_and_light_to_bin_string([Char | Rest]) when Char == ?LIGHT->
  "1" ++ dark_and_light_to_bin_string(Rest);
dark_and_light_to_bin_string([Char | Rest]) when Char == ?DARK -> %% if not ?LIGHT, then will be ?DARK
  "0" ++ dark_and_light_to_bin_string(Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% READING INPUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

readInput() ->
  [EnhancementLine | [_ | ImageLines] ] = readlines("../resources/day20.in"),
  { EnhancementLine, ImageLines }.

%% Adapt the input to make the problem a bit simpler
with_border_of(Original, Char) ->
  Size = length(Original),
  AllSameCharRow = [ Char || _ <- lists:seq(1, Size + 2)],
  [AllSameCharRow] ++ [ with_border_on_line_of(Line, Char) || Line <- Original ] ++ [AllSameCharRow].

with_border(Original) ->
  with_border_of(Original, ?DARK).

with_border_on_line_of(Line, Char) ->
  [Char] ++ Line ++ [Char].
with_border_on_line(Line) ->
  with_border_on_line_of(Line, ?DARK).

readlines(FileName) ->
  {ok, Device} = file:open(FileName, [read]),
  try get_all_lines(Device)
  after file:close(Device)
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof -> [];
    Line -> [string:trim(Line) | get_all_lines(Device)]
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRINTING OUT DATA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_image(Text, Image) ->
  [ println(Row) || Row <- Image],
  io:format("~p: Size: ~p x ~p~n", [Text, length(Image), length(nth_element(0, Image))]).

print_image(Image) ->
  print_image("Image: ", Image).
