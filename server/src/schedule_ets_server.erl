-module(schedule_ets_server).

-include("schedule.hrl").

-behavior(gen_server).



%% API Exports
-export([
         start_link/0,
         insert_schedule_item/1,
         get_schedule_for_hash/1,
         die/0
        ]).

%% GenServer Callback Exports
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2
        ]).

%% Function specs Private Type Definitions
-spec insert_schedule_item(schedule_item()) -> ok.
-spec get_schedule_for_hash(term()) -> {ok, schedule_item()};
                           (term()) -> {error, term()}.

-record(state, {init=true, table_id}).

-define(SERVER, ?MODULE).

%% API
start_link() ->
    gen_server:start_link({local, ?SERVER}, [], []).

insert_schedule_item(ScheduleItem) ->
    gen_server:cast(?SERVER, {insert, ScheduleItem}).

get_schedule_for_hash(Hash) ->
    gen_server:call(?SERVER, {get, Hash}).

die() ->
    gen_server:cast(?SERVER, die).
%% Gen Server Callbacks
init([]) ->
    {ok, #state{}}.

handle_call({get, Hash}, _From, State) ->
    % TODO: Implement ETS Query
    {reply, [], State};
handle_call(_Rquest, _From, State) ->
    {reply, ok, State}.

handle_cast(die, State) ->
    exit(killed),
    {noreply, State};
handle_cast({insert, ScheduleItem}, State) ->
    TableId = State#state.table_id,
    case ets:insert_new(TableId, ScheduleItem) of
        false ->
            % TODO: Actual logging here
            io:format("Could not Insert Hash");
        _ ->
            {noreply, State}
    end;
      
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'ETS-TRANSFER', TableId, Pid, _Data}, State) ->
    {noreply, State#state{table_id=TableId}};
handle_info(_info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.








