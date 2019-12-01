-module(schedule_ets_server).

-include("schedule.hrl").

-behavior(gen_server).



%% API Exports
-export([
         start_link/0,
         insert/1,
         get_all/0,
         die/0
        ]).

%% GenServer Callback Exports
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

%% Function specs Private Type Definitions
-spec insert(schedule_item()) -> ok.
-spec get_all() -> [schedule_item()].

-record(state, {init=true, table_id}).

-define(SERVER, ?MODULE).

%% API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

insert(ScheduleItem) ->
    gen_server:cast(?SERVER, {insert, ScheduleItem}).

get_all() ->
    gen_server:call(?SERVER, get_all).

die() ->
    gen_server:cast(?SERVER, die).

%% Gen Server Callbacks
init([]) ->
    {ok, #state{}}.

handle_call({get, Hash}, _From, State) ->
    TableId = State#state.table_id,
    Result = ets:lookup(TableId, Hash),
    {reply, Result, State};
handle_call(get_all, _From, State) ->
    TableId = State#state.table_id,
    Records = get_records(TableId),
    {reply, Records, State};
handle_call(_Rquest, _From, State) ->
    {reply, ok, State}.

handle_cast(die, State) ->
    exit(killed),
    {noreply, State};
handle_cast({insert, ScheduleItem}, State) ->
    TableId = State#state.table_id,
    case ets:insert_new(TableId, ScheduleItem) of
        false ->
            io:format("Could not Insert Hash"),
            {noreply, State};
        _ ->
            {noreply, State}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'ETS-TRANSFER', TableId, Pid, _Data}, State) ->
    io:format("TableId(~p) Transferred From Server(~p)~n", [TableId, Pid]),
    {noreply, State#state{table_id=TableId}};
handle_info(_info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_records(TableId) ->
    case get_first(TableId) of
        empty_table ->
            io:format("Table(~p) Is Empty", [TableId]);
        {Key, Records} ->
            get_tail(TableId, Key, Records)
    end.

get_first(TableId) ->
    case ets:first(TableId) of
        '$end_of_table' ->
            emtpy_table;
        Key ->
            Records = ets:lookup(TableId, Key),
            {Key, Records}
     end.


get_tail(TableId, Prev, Records) ->
    case ets:next(TableId, Prev) of
        '$end_of_table' ->
            Records;
        Key ->
            Results = ets:lookup(TableId, Key),
            get_tail(TableId, Key, Records ++ Results)
    end.
            







