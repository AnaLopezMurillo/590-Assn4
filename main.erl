-module(main).
-compile([export_all]).

start() ->  
    io:format("Starting"),
    serv1 = spawn(?MODULE, loop1, []),

    
    serv1 ! stop.

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