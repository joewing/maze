%%
%% Maze generator in Erlang.
%% Joe Wingbermuehle
%% 2010-04-19 <> 2010-04-23
%%

-module(maze).
-export([start_maze_server/0, maze_server/0, main/0]).

-define(HEIGHT, 23).    % Height of the maze, must be odd.
-define(WIDTH, 39).     % Width of the maze, must be odd.

%% Get the array index for coordinates in the maze.
get_index(X, Y) -> Y * ?WIDTH + X.

%% Look up an element in the maze.
get_element(Maze, X, Y) -> array:get(get_index(X, Y), Maze).

%% Set an element in the maze.
set_element(Maze, X, Y, V) -> array:set(get_index(X, Y), V, Maze).

%% Initialize a maze.
init_maze() ->
   M0 = array:new(?WIDTH * ?HEIGHT, {default, wall}),
   M1 = lists:foldl(fun({X, Y}, M) -> set_element(M, X, Y, border) end,
                    M0, [ {X, Y} || X <- lists:seq(0, ?WIDTH - 1),
                                    Y <- lists:seq(0, ?HEIGHT - 1),
                                    ((X =:= 0) or (X =:= ?WIDTH - 1)
                                      or (Y =:= 0) or (Y =:= ?HEIGHT - 1)) ]),
   set_element(M1, 1, 1, space).

%% Show a block.
show_item(space)     -> io:fwrite("  ");    % Empty space
show_item(solution)  -> io:fwrite("++");    % Solution
show_item(_)         -> io:fwrite("[]").    % Wall (1 or 2).

%% Show a line of the maze.
show_line(Maze, Y) ->
   lists:foldl(fun (X, _) -> show_item(get_element(Maze, X, Y)) end,
               0, lists:seq(0, ?WIDTH - 1)).

%% Show a maze.
show_maze(Maze) ->
   lists:foldl(fun(Y, _) -> show_line(Maze, Y), io:nl() end,
               0, lists:seq(0, ?HEIGHT - 1)).

%%
carve_maze_helper(Maze, X, Y, Dir) ->
   { Xd, Yd } =   case Dir of
                     0 -> { -1,  0 };
                     1 -> {  0, -1 };
                     2 -> {  1,  0 };
                     3 -> {  0,  1 }
                  end,
   case get_element(Maze, X + Xd, Y + Yd) of
      wall ->
         { Nx, Ny } = { X + 2 * Xd, Y + 2 * Yd },
         case get_element(Maze, Nx, Ny) of
            wall ->
               M0 = set_element(Maze, X + Xd, Y + Yd, space),
               M1 = set_element(M0, Nx, Ny, space),
                  { true, carve_maze(M1, Nx, Ny) };
            _ ->  { false, Maze }
         end;
      _ ->
         { false, Maze }
   end.

%% Carve a route at X, Y with a starting direction.
find_route(Maze, _X, _Y, _D0, 0) -> { false, Maze };
find_route(Maze, X, Y, D0, Dir) ->
   case carve_maze_helper(Maze, X, Y, (D0 + Dir) rem 4) of
      { false, _ } -> find_route(Maze, X, Y, D0, Dir - 1);
      { true,  M } -> { true, M }
   end.

%% Carve a maze at X, Y.
carve_maze(Maze, X, Y) ->
   D0 = random:uniform(4) - 1,
   { _, M } = find_route(Maze, X, Y, D0, 4),
   M.

%% Generate a random maze.
generate_maze(Maze) ->
   random:seed(now()),
   lists:foldl(fun({X, Y}, M) -> carve_maze(M, X, Y) end,
               Maze, [ {X, Y} || X <- lists:seq(1, ?WIDTH - 2),
                                 Y <- lists:seq(1, ?HEIGHT - 2),
                                 (X rem 2 =:= 1),
                                 (Y rem 2 =:= 1) ]).

%% Helper method for finding a maze solution.
try_route(Maze, X, Y) ->
   case get_element(Maze, X, Y) of
      space  ->
         M0 = set_element(Maze, X, Y, solution),
         find_solution(M0, X, Y, 0);
      _  -> { false, Maze }
   end.

%% Find a solution starting at X, Y.
find_solution(Maze, _X, _Y, 4) -> { false, Maze };
find_solution(Maze, X, Y, Dir) ->
   case { X, Y } of
      { ?WIDTH - 2, ?HEIGHT - 2} -> { true, Maze };
      _ ->
         { Status, M } =
            case Dir of
               0 -> try_route(Maze, X + 1, Y);
               1 -> try_route(Maze, X - 1, Y);
               2 -> try_route(Maze, X, Y + 1);
               3 -> try_route(Maze, X, Y - 1)
            end,
         case Status of
            true -> { true, M };
            false -> find_solution(Maze, X, Y, Dir + 1)
         end
   end.

%% Solve a maze.
solve_maze(Maze) ->
   M0 = set_element(Maze, 0, 1, border),
   { _, M1 } = find_solution(M0, 1, 1, 0),
   M2 = set_element(M1, 1, 1, solution),
   M3 = set_element(M2, 1, 0, solution),
   M4 = set_element(M3, ?WIDTH - 1, ?HEIGHT - 2, solution),
   M4.

%% Maze server.
maze_server() ->
   receive
      { generate, From, _ } ->
         M0 = init_maze(),
         M1 = generate_maze(M0),
         M2 = set_element(M1, 0, 1, space),
         M3 = set_element(M2, ?WIDTH - 1, ?HEIGHT - 2, space),
         From ! { ok, M3 },
         maze_server();
      { solve,    From, M } ->
         From ! { ok, solve_maze(M) },
         maze_server();
      { show,     From, M } ->
         show_maze(M),
         From ! { ok, [] },
         maze_server();
      { stop,     From, _ } ->
         From ! { ok, [] };
      { _,  From, _ } ->
         From ! { error, "invalid command" }
   end.

%% Start the maze server.
start_maze_server() -> spawn(?MODULE, maze_server, []).

%% Entry point to test the maze server.
main() ->
   Pid = start_maze_server(),
   Pid ! { generate, self(), [] },
   Maze = receive
      { ok, M } -> M
   end,
   io:fwrite("Maze:~n"),
   Pid ! { show, self(), Maze },
   receive
      { ok, _ } -> ok
   end,
   Pid ! { solve, self(), Maze },
   Solved = receive
      { ok, S } -> S
   end,
   io:fwrite("Solution:~n"),
   Pid ! { show, self(), Solved },
   receive
      { ok, _ } -> ok
   end,
   Pid ! { stop, self(), [] },
   receive
      { ok, _ } -> ok
   end.

