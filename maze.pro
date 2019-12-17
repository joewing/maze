% Maze Generator in Prolog
% Joe Wingbermuehle
% 2010-09-26

% Size of the maze.  Must be odd.
get_size(39, 23).

% Get an offset into the list representing the maze.
get_offset(X, Y, Offset) :-
   get_size(W, _),
   Offset is Y * W + X.

% Split a list into the part before and after the specified index.
split(0, [_|A], [], A).
split(N, [H|T], [H|B], A) :-
   NextN is N - 1,
   split(NextN, T, B, A).

% Get the value of the maze.
get_value(X, Y, M, V) :-
   get_offset(X, Y, Offset),
   nth0(Offset, M, V).

% Set a value in the maze.
set_value(X, Y, V, In, Out) :-
   get_offset(X, Y, Offset),
   split(Offset, In, Before, After),
   append(Before, [V], Temp),
   append(Temp, After, Out).

% Display the maze.
show_block(wall)  :- write('[]').
show_block(space) :- write('  ').
show_maze(0, _) :- true.
show_maze(X, [H|T])  :-
   show_block(H),
   get_size(W, _),
   NX is X - 1,
   ( NX mod W =:= 0 -> nl, show_maze(NX, T); show_maze(NX, T) ).
show_maze(M) :-
   get_size(W, H),
   T is W * H,
   show_maze(T, M).

% Initialize the maze.
init_maze(0, []).
init_maze(X, M) :-
   T is X - 1,
   init_maze(T, S),
   M = [wall|S].
init_maze(M) :-
   get_size(W, H),
   T is W * H,
   init_maze(T, Temp),
   set_value(1, 1, space, Temp, M).

% Carve the maze at the specified coordinates.
carve_maze_at(X, Y, Dir, Count, In, Out) :-
   ( Dir =:= 0 -> DX is  1, DY is  0;
     Dir =:= 1 -> DX is  0, DY is  1;
     Dir =:= 2 -> DX is -1, DY is  0;
     Dir =:= 3 -> DX is  0, DY is -1 ),
   X1 is X  + DX, Y1 is Y  + DY,
   X2 is X1 + DX, Y2 is Y1 + DY,
   get_size(W, H),
   ( X2 > 0, Y2 > 0, X2 < W, Y2 < H,
     get_value(X1, Y1, In, wall),
     get_value(X2, Y2, In, wall) ->
      set_value(X1, Y1, space, In, M1),
      set_value(X2, Y2, space, M1, M2),
      NextDir is random(3),
      carve_maze_at(X2, Y2, NextDir, 0, M2, Out)
   ;
      ( Count < 3 ->
         NextCount is Count + 1,
         NextDir is (Dir + 1) mod 4,
         carve_maze_at(X, Y, NextDir, NextCount, In, Out)
      ;
         Out = In
      )
   ).

% Carve the maze.
carve_maze(X, Y, In, Out) :-
   Dir is random(3),
   get_size(W, H),
   ( X < W, Y < H ->
      carve_maze_at(X, Y, Dir, 0, In, Temp),
      NextX is X + 2,
      carve_maze(NextX, Y, Temp, Out)
   ;
      ( X =:= W, Y < H ->
         carve_maze_at(X, Y, Dir, 0, In, Temp),
         NextY is Y + 2,
         carve_maze(1, NextY, Temp, Out)
      ;
         set_value(1, 0, space, In, M1),
         set_value(W - 2, H - 1, space, M1, Out)
      )
   ).
carve_maze(In, Out) :- carve_maze(1, 1, In, Out).

% Generate and display a maze.
maze :-
   init_maze(EmptyMaze),
   carve_maze(EmptyMaze, Maze),
   show_maze(Maze),
   halt.

maze.

