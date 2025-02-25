-module(main).
-compile([export_all]).

start() ->  
    io:format("~nStarting...~n"),
    % take in a user message
    {ok, Msg} = io:read("Enter something: "),

    Serv1 = spawn(?MODULE, loop1, []),

    Serv1 ! {self(), Msg},
    receive
        {Serv1, Msg1} ->
            io:format("~n(Serv1) responded with: ~w~n", [Msg1])
    end,

    timer:sleep(2000),
    
    Serv1 ! stop.

loop1() -> 
    % this is serv1
    receive
        {From, Msg} ->
            io:format("(Serv1) received: ~w~n", [Msg]),
            From ! {self(), Msg},
            loop1();
        stop ->
            true
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