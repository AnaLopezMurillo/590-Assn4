-module(main).
-compile([export_all]).

start() ->  
    io:format("~nStarting...~n"),
    % take in a user message
    {ok, Input} = io:read("Enter something: "),

    Serv1 = spawn(?MODULE, loop1, []),

    Serv1 ! {self(), Input},
    receive
        {Serv1, Response} ->
            io:format("~n(Serv1) responded with: ~w~n", [Response])
    end,

    Serv1 ! stop.

loop1() -> 
    % this is serv1
    receive
        {From, Msg} ->
            io:fwrite("(Serv1) received: ~w of size ~w~n", [Msg, tuple_size(Msg)]),
            if 
                tuple_size(Msg) == 3 ->
                    io:format("Entered a tuple of size 3.~n"),
                    Op = element(1, Msg),
                    N1 = element(2, Msg),
                    N2 = element(3, Msg),
                    {OpName, Result} = serv1_compute(Op, N1, N2),
                    io:format("~p ~s ~p = ~p", [N1, OpName, N2, Result]);
                tuple_size(Msg) == 2 -> 
                    io:format("Entered size 2.~n");
            true ->
                io:format("Throw an error here~n")
            end,

            From ! {self(), Msg},
            loop1();
        stop ->
            true
    end.

serv1_compute(Op, N1, N2) -> 
    % add, sub, mult, div
    case Op of 
        'add' -> {"+", (N1 + N2)};
        'sub' -> {"-", (N1 - N2)};
        'mult' -> {"*", (N1 * N2)};
        'div' -> {"/", (N1 / N2)};
    _ -> 
        % this is div case bc "div" is a reserved keyword in erlang lol
        io:format("~n We are here"),
        try apply(erlang, Op, [N1, N2]) of
            Result -> {"/", (Result)}
        catch
            _:_ -> io:format("ERROR HERRRREEEEEE")
        end
    end.

loop2() -> 
    % this is serv2
    receive
        {From, Msg} ->
            io:format("(Serv2) received: ~w~n", [Msg]),
            From ! {self(), Msg},
            loop2();
        stop ->
            true
    end.


loop3() -> 
    % this is serv3
    receive
        {From, Msg} ->
            io:format("(Serv3) received: ~w~n", [Msg]),
            From ! {self(), Msg},
            loop3();
        stop ->
            true
    end.