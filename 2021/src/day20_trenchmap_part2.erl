%%--- Part Two ---
%%You still can't quite make out the details in the image. Maybe you just didn't enhance it enough.
%%
%%If you enhance the starting input image in the above example a total of 50 times, 3351 pixels are lit in the final output image.
%%
%%Start again with the original input image and apply the image enhancement algorithm 50 times. How many pixels are lit in the resulting image?

%% run with: `echo "cd('./2021/src/'). c(day20_trenchmap_part2). day20_trenchmap_part2:main(). init:stop()." | erl`

%% had to name this file like this because of Erlang's module naming
-module(day20_trenchmap_part2).
-compile([export_all]). %% can export all the functions

print(What) -> io:format("~p", [What]).
println(What) -> io:format("~p~n", [What]).

-define(DARK, 46). %% .
-define(LIGHT, 35). %% #

main() ->
  { Enhancement, Image } = readInput(),

  StartTime = os:system_time(millisecond),
  Enhanced = enhance_n_times(50, Image, Enhancement),
  EndTime = os:system_time(millisecond),

  %% print_image("Enhanced image", Enhanced),
  println("Lit pixels: " ++ integer_to_list(count_light_pixels(Enhanced))),
  io:format("Executed in ~pms~n", [EndTime-StartTime]),
  ok.

%% trying a different approach, where we add the 2 borders (due to the input, in every turn they switch from . to # and back to . in a loop
enhance_n_times(N, Image, Enhancement) ->
  enhance_n_times_(N, 0, Image, Enhancement).

enhance_n_times_(N, I, Image, _) when N == I ->
  Image;
enhance_n_times_(N, I, Image, Enhancement) ->
  BorderChar = if I rem 2 == 0 -> ?DARK; true -> ?LIGHT end,
  WithBorder = with_n_borders_of(2, Image, BorderChar),
  enhance_n_times_(N, I+1, enhance(WithBorder, Enhancement), Enhancement).

with_n_borders_of(0, Image, _) ->
  Image;
with_n_borders_of(N, Image, Char) ->
  with_n_borders_of(N-1, with_border_of(Image, Char), Char).

enhance(Image, _) when length(Image) < 3 ->
  [];
enhance(Image, Enhancement) ->
  [FirstRow | [SecondRow | [ThirdRow | _]]] = Image,
  EnhancedRow = enhance_row(FirstRow, SecondRow, ThirdRow, Enhancement),
  [ EnhancedRow | enhance(tail(Image), Enhancement)].
enhance_row(FirstRow, _, _, _) when length(FirstRow) < 3 ->
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

