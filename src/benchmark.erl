%%%-------------------------------------------------------------------
%%% @author nietaki
%%% @copyright (C) 2016, nietaki.github.io
%%% @doc
%%%
%%% @end
%%% Created : 16. May 2016 13:21
%%%-------------------------------------------------------------------
-module(benchmark).
-author("nietaki").

%% API
-export([run/1, move_around/1]).


% for now server should be running already when this is called
run(ClientCount) -> 
    %spawn_server(),
    FirstClient = spawn_client(first_client),
   
    %getting some initial text
    perform_x_times(100, fun (_) -> client_editor:send_char(FirstClient, $x) end),
  
    %spawning the chatty clients
    perform_x_times(ClientCount, fun(No) -> spawn_client_number(No) end),
    timer:sleep(500),

    perform_x_times(ClientCount, fun(No) -> make_chatty(No) end),
    
    %cleanup
    kill_process(FirstClient),
    ok. 

spawn_client_number(Number) -> 
    Name = get_atom_for_integer(Number),
    ClientPid = spawn_client(Name),
    ok.

make_chatty(Number) ->
    Name = get_atom_for_integer(Number),
    spawn_link(benchmark, move_around, [Name]). 

move_around(ClientPid) ->
    give_random_movement_command(ClientPid),
    move_around(ClientPid).

give_random_movement_command(ClientPid) ->
    Directions = [up, down, left, right],
    Direction = get_random_element_of_list(Directions),
    client_editor:send_keystroke(ClientPid, Direction).

get_random_element_of_list(List) ->
    Length = length(List),
    Pos = rand:uniform(Length),
    lists:nth(Pos, List).

perform_x_times(0, _Fun) -> ok;
perform_x_times(X, Fun) ->
    Fun(X),
    perform_x_times(X - 1, Fun).
    
    
    
get_atom_for_integer(Integer) when is_integer(Integer) ->
    list_to_atom("atom_" ++ integer_to_list(Integer)).
    
spawn_server() ->
    process_flag(trap_exit, true),
    {ok, ServerPid} = ledgerServer:start_link(),
    ServerPid.

spawn_client(ArbitraryName) ->
    {ok, ClientPid} = client_editor:start_link(fun initializeNop/1, fun repaintNop/2, ArbitraryName),
    ClientPid.

kill_process(Pid) ->
    process_flag(trap_exit, true),
    exit(Pid, kill). %% brutal kill!

initializeNop(_) -> {ok, {20, 20}}.
repaintNop(_, _) -> ok.
