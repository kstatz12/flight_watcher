-module(schedule_sup).

-behavior(gen_server).

-export([
         start_link/0
        ]).

-export([
         init/1
        ]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
   {ok, []}.


