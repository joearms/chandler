-module(erl3).
-export([start/0, pre/1]).

%% This demo assumes that all your code
%% the code paths to ezwebframe and simple_demo have beed setup 

start() ->
    %% compile all the code in the subdirectories
    %% and set paths appropriately
    io:format("a simple_demo....~n"),
    twit_store:start_link(),
    Port = 7003,
    io:format("Load the page http://localhost:~p/ in your browser~n",[Port]),
    erl3_webserver:start_link(fun dispatch/1, Port).

%% dispatch maps names in the HTML onto fixed paths 

dispatch(F) ->
    F1 = dispatch1(F),
    io:format("erl3::dispatch ~s => ~s~n",[F,F1]),
    F1.

dispatch1("/" ++ F) ->
    filename:dirname(code:which(?MODULE)) ++ "/" ++ F.

pre(X) ->
    erl3_webserver:pre(X).



