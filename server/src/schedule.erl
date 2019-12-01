-module(schedule).

get_schedule_ets_server() ->
    case whereis(schedule_ets_server) of
        undefined ->
            timer:sleep(1),
            get_schedule_ets_server();
        Pid -> Pid
    end.


