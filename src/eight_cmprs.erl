-module(eight_cmprs).
-author("g.lominashvili@gmail.com").
-export([start/0, start/2, motherofnodes/2, motherofnodes/3, matches/2, print/1, worker/4, moves/2, move/3, index/2, heuristicum/2, manhattan/2, hamming/2, get_timestamp/0]).
-define(X,0).
get_timestamp()->{Mega, Sec, Micro} = os:timestamp(),(Mega*1000000 + Sec)*1000 + round(Micro/1000).
matches(M1, M2)->M1 =:= M2.
print(Matrix) ->
    io:format("~n"),
    lists:foreach(fun(El) ->
                String = lists:foldl(fun(X, Accm) ->
                            Accm++"|"++integer_to_list(X)++"|"
                    end,"",El),
                io:format("~s~n", [String])
        end, Matrix),
    io:format("~n").
index(Matrix, Elem) ->
    Columns = length(lists:nth(1, Matrix)),
    FlatMatrix = lists:flatten(Matrix),
    {Index, _} = lists:foldl(fun(X,Data) ->
                {I, Status} = Data,        
                case Status of
                    found ->
                        Data;
                    _ ->
                        case X == Elem of
                            true -> {I, found};
                            _ -> {I+1, notf}
                        end
                end
        end, {1, notf}, FlatMatrix),
    case Index rem Columns of
        0 ->
            {Index div Columns, Columns};
        _ ->
            {(Index div Columns)+1, Index rem Columns}
    end.
moves(Matrix, El) ->
    Columns = length(lists:nth(1, Matrix)),
    Rows = length(Matrix),
    {X, Y} = ?MODULE:index(Matrix, El),
    LeftRight = case X of
        1 -> [down];
        Columns -> [up];
        _ -> [down, up]
    end,
    UpDown = case Y of
        1 -> [right];
        Rows -> [left];
        _ -> [right, left]
    end,
    lists:merge(LeftRight, UpDown).
move(Matrix, El, Dir) ->
    {X, Y} = ?MODULE:index(Matrix, El), 
    {X1, Y1} = case Dir of
        up -> {X-1, Y};
        down -> {X+1, Y};
        right -> {X, Y+1};
        left -> {X, Y-1}
    end,
    SwapEl = lists:nth(Y1, lists:nth(X1, Matrix)),Row = lists:nth(X1, Matrix),Changed = lists:sublist(Row, Y1-1) ++ [El] ++ lists:nthtail(Y1, Row),Matrix1 = lists:sublist(Matrix, X1-1) ++ [Changed] ++ lists:nthtail(X1, Matrix),
    RowSwp = lists:nth(X, Matrix1),ChangedSwp = lists:sublist(RowSwp, Y-1) ++ [SwapEl] ++ lists:nthtail(Y, RowSwp),Matrix2 = lists:sublist(Matrix1, X-1) ++ [ChangedSwp] ++ lists:nthtail(X, Matrix1),
    Matrix2.
manhattan(Goal, Matrix) ->
    FlatMatrix = lists:flatten(Matrix),
    lists:foldl(fun(X, Sum) when X /= ?X ->
                {X1, Y1} = ?MODULE:index(Goal, X),                                                
                {X2, Y2} = ?MODULE:index(Matrix, X),
                Sum+(abs(X2 - X1) + abs(Y2 - Y1));
            (_, Sum) -> Sum end,0,FlatMatrix).
hamming(Goal, Matrix) ->
    FlatMatrix = lists:flatten(Matrix),
    lists:foldl(fun(X, Sum) when X /= ?X ->
                {X1, Y1} = ?MODULE:index(Goal, X),                                                
                {X2, Y2} = ?MODULE:index(Matrix, X),
                case {X1, Y1} == {X2, Y2} of
                    true -> Sum;
                    _ -> Sum+1
                end;
            (_, Sum) -> Sum end, 0, FlatMatrix).
heuristicum(Goal, Matrix)->?MODULE:manhattan(Goal, Matrix) + ?MODULE:hamming(Goal, Matrix).
start() -> start([[1, 2, 3], [8, ?X, 4], [7, 6, 5]], [[2, 8, 3], [1, 6, 4], [7, ?X, 5]]).
start(Goal, StartState) ->
    io:format("Goal Matrix! ~n"),
    ?MODULE:print(Goal),
    io:format("Currsent State Matrix! ~n"),
    ?MODULE:print(StartState),
    case matches(Goal, StartState) of
        true -> io:format("StartState Matrix is already Goal Matrix!");
        _ -> io:format("Spawning Mother of Nodes!~n Time is ~p~n", [?MODULE:get_timestamp()]), spawn(?MODULE, motherofnodes, [Goal, StartState])
    end.


motherofnodes(Goal, StartState) ->
    Moves = ?MODULE:moves(StartState, ?X),Nodes = [spawn(?MODULE, worker, [self(), Goal, StartState, Direction]) || Direction <- Moves],
    motherofnodes(Nodes, Goal, []).


motherofnodes(Nodes, Goal, HeurMatrices) ->
    case length(Nodes) == length(HeurMatrices) of
        true ->
            {_ ,GoodMatrices} = lists:foldl(fun(El, Data) ->
                        {Score, _} = El,
                        {CurrScore, HMList} = Data,
                        case Score == CurrScore of
                            true -> {CurrScore, lists:append(HMList, [El])};
                            _ ->
                                case Score < CurrScore of
                                    true -> {Score, [El]};
                                    _ -> Data
                                end
                        end
                end, {10000000,[]}, HeurMatrices),
            io:format("Good Matrices are ~p~n", [GoodMatrices]),
            NewNodes = lists:foldl(fun(El, Data) ->
                        {_, Matrix} = El,
                        Moves = ?MODULE:moves(Matrix, ?X),
                        LocalNodes = [spawn(?MODULE, worker, [self(), Goal, Matrix, Direction]) || Direction <- Moves],
                        lists:append(Data, LocalNodes)
                end, [], GoodMatrices),
            motherofnodes(NewNodes, Goal, []);

        _ ->
            receive
                {HeurScore, Matrix} -> motherofnodes(Nodes, Goal, lists:append(HeurMatrices, [{HeurScore, Matrix}]));
                {found, _, _} -> ok;
                _ -> motherofnodes(Nodes, Goal, HeurMatrices)
            end
    end.

worker(ParentPid, Goal, Matrix, Direction) ->
    Moved = ?MODULE:move(Matrix, ?X, Direction),
    HeurScore = ?MODULE:heuristicum(Goal, Matrix),
    case ?MODULE:matches(Goal, Moved) of
        true -> ParentPid ! {found, HeurScore, Moved}, io:format("-->>Node ~p found Goal Matrix <<--~n Time is ~p~n", [self(), ?MODULE:get_timestamp()]),?MODULE:print(Moved);
        _ -> ParentPid ! {HeurScore, Moved}
    end.
