-module(echo).
-export([start/0, stop/0]).

echo() ->
    receive
        {Pid, Msg} -> Pid ! Msg, echo();
        stop       -> ok
    end.

start() ->
    register(echo, spawn(fun() -> echo() end)).

stop() ->
    echo ! stop.
