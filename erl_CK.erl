-module(server_chain).
-export([start/0, serv1/0, serv2/0, serv3/1]).

% serv1 handles arithmetic operations and forwards unhandled messages to serv2
serv1() ->
    receive
        {add, X, Y} -> io:format("(serv1) ~p + ~p = ~p~n", [X, Y, X + Y]), serv1();
        {sub, X, Y} -> io:format("(serv1) ~p - ~p = ~p~n", [X, Y, X - Y]), serv1();
        {mult, X, Y} -> io:format("(serv1) ~p * ~p = ~p~n", [X, Y, X * Y]), serv1();
        {div, X, Y} -> io:format("(serv1) ~p / ~p = ~p~n", [X, Y, X / Y]), serv1();
        {neg, X} -> io:format("(serv1) -~p = ~p~n", [X, -X]), serv1();
        {sqrt, X} -> io:format("(serv1) sqrt(~p) = ~p~n", [X, math:sqrt(X)]), serv1();
        halt -> io:format("(serv1) Halting...~n"), serv2 ! halt; % forwards halt to serv2
        Msg -> serv2 ! Msg, serv1() % forwards unhandled messages to serv2
    end.

% serv2 processes lists and forwards unhandled messages to serv3
serv2() ->
    receive
        [H | T] when is_integer(H) ->
            Sum = lists:sum([X || X <- [H | T], is_number(X)]),
            io:format("(serv2) Sum: ~p~n", [Sum]),
            serv2();
        [H | T] when is_float(H) ->
            Product = lists:foldl(fun(X, Acc) when is_number(X) -> X * Acc; (_, Acc) -> Acc end, 1, [H | T]),
            io:format("(serv2) Product: ~p~n", [Product]),
            serv2();
        halt -> io:format("(serv2) Halting...~n"), serv3(0); % forwards halt to serv3
        Msg -> serv3(0) ! Msg, serv2() % forwards unhandled messages to serv3
    end.

% serv3 handles errors and keeps count of unprocessed messages
serv3(Count) ->
    receive
        {error, Msg} -> io:format("(serv3) Error: ~p~n", [Msg]), serv3(Count);
        halt -> io:format("(serv3) Halting... Not handled messages: ~p~n", [Count]); % prints count before halting
        Msg -> io:format("(serv3) Not handled: ~p~n", [Msg]), serv3(Count + 1) % increments count for unhandled messages
    end.

% start function initializes processes and starts user input loop
start() ->
    Serv1 = spawn(fun serv1/0),
    Serv2 = spawn(fun serv2/0),
    Serv3 = spawn(fun() -> serv3(0) end),
    loop(Serv1).

% loop continuously takes user input and sends messages to serv1
loop(Serv1) ->
    io:format("Enter message: "),
    case io:get_line("") of
        "all_done\n" -> Serv1 ! halt, io:format("Exiting...~n"); % sends halt message to chain
        Input ->
            Serv1 ! parse_input(Input), 
            loop(Serv1)
    end.

% parses user input into appropriate messages for the servers
parse_input(Input) ->
    case string:tokens(string:trim(Input), " ") of
        ["add", X, Y] -> {add, list_to_integer(X), list_to_integer(Y)};
        ["sub", X, Y] -> {sub, list_to_integer(X), list_to_integer(Y)};
        ["mult", X, Y] -> {mult, list_to_integer(X), list_to_integer(Y)};
        ["div", X, Y] -> {div, list_to_integer(X), list_to_integer(Y)};
        ["neg", X] -> {neg, list_to_integer(X)};
        ["sqrt", X] -> {sqrt, list_to_integer(X)};
        List when is_list(List) -> [list_to_number(X) || X <- List];
        _ -> {error, "Invalid Input"} % returns an error tuple for invalid inputs
    end.

% converts a string to a number (integer or float)
list_to_number(X) ->
    case string:to_float(X) of
        {error, _} -> list_to_integer(X);
        {F, _} -> F
    end.
