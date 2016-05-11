-module(erlangEditor_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    %Editor = {erlangEditor, {erlangEditor, cecho_test, []}, permanent, 20, worker, [erlangEditor]},
    Display = {client_editor, {client_editor, start_link, []}, permanent, 20, worker, [erlangEditor]},
    Children = [Display],
    {ok, { {one_for_one, 5, 10}, Children}}.

