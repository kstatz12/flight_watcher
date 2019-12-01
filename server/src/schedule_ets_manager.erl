-module(schedule_ets_manager).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-record(state, {table_id}).

-export([
         init/1,
         start_link/0,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).


gift() ->
    gen_server:cast(?SERVER, gift).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_) ->
    process_flag(trap_exit, true),
    gift(),
    {ok, #state{}}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(gift, State) ->
    SRV  = wait_for_server(),
    link(SRV),
    TableId = ets:new(schedule_table, [set, private]),
    ets:setopts(TableId, {heir, self(), []}),
    ets:give_away(TableId, SRV, []),
    {noreply, State#state{table_id=TableId}};
handle_cast(_, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, killed}, State) ->
    TableId = State#state.table_id,
    io:format("Server(~p) has died, ETS Table With Id: ~p~n", [Pid, TableId]),
    {noreply, State};
handle_info({'ETS-TRANSFER', TableId, Pid, Data}, State) ->
    SRV = wait_for_server(),
    io:format("Server(~p) Died, Reassigning Table(~p) to ~p~n", [Pid, TableId, SRV]),
    link(SRV),
    ets:give_away(TableId, SRV, Data),
    {noreply, State#state{table_id=TableId}}.

wait_for_server() ->
    case whereis(schedule_ets_server) of
        undefined ->
            timer:sleep(1),
            wait_for_server();
        Pid -> Pid
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
