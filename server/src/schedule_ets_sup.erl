-module(schedule_ets_sup).

-behavior(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Server = ?CHILD(schedule_ets_server, worker),
    Manager = ?CHILD(schedule_ets_manager, worker),
    {ok, { {one_for_one, 5, 10}, [Server, Manager]} }.
