-module(srtp_worker).
-behaviour(gen_statem).
-mode(compile).
-compile(export_all).
-include("../logger.hrl").

%%====================================================================
%% API functions
%%====================================================================
start_link(#{}, #{}) ->
    ?Log(gen_statem:start_link({local, ?MODULE}, ?MODULE, #{}, [])).

stop(Pid) ->
    ?Log(gen_statem:stop(Pid)).


%%====================================================================
%% Behaviour callbacks
%%====================================================================
init(State) ->
    process_flag(trap_exit, true),
    ?Log(State),
    {ok, listening, State}.

callback_mode() ->
    state_functions.

listening(cast, srtp, State) ->
    ?Log(received, srtp),
    keep_state_and_data;

listening(cast, Msg, State) ->
    ?Log(Msg, State),
    keep_state_and_data.

terminate(Reason, StateName, State) ->
    ?Log(Reason, StateName, State),
    ok.


%%====================================================================
%% Internal functions
%%====================================================================
