-module(schedule_server).

-behavior(gen_server).

-export([init/1]).

init(_) ->
    {ok, []}.
