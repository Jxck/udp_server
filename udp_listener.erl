-module(udp_listener).
-behaviour(gen_statem).
-mode(compile).
-compile(export_all).
-include("../logger.hrl").

-define(TIMEOUT, 10000).

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

    % spawn worker and link
    {ok, DTLS} = ?Log(dtls_worker_sup:start_child(#{})),
    {ok, SRTP} = ?Log(srtp_worker_sup:start_child(#{})),

    true = ?Log(link(DTLS)),
    true = ?Log(link(SRTP)),

    NextState = State#{
                  dtls_worker => DTLS,
                  srtp_worker => SRTP
                 },

    {ok, listening, NextState}.

callback_mode() ->
    state_functions.


listening(info, {'EXIT', PID, Reason}, State) ->
    ?Log(PID, Reason),
    ?Log(State),
    {stop, {shutdown, child_termination}};

listening(timeout, ?TIMEOUT, State) ->
    {stop, {shutdown, timeout}};

listening(cast, udp, State) ->
    ?Log(received, udp),
    {keep_state_and_data, ?TIMEOUT};

listening(cast, dtls, #{dtls_worker := DTLS}=State) ->
    gen_statem:cast(DTLS, dtls),
    {keep_state_and_data, ?TIMEOUT};

listening(cast, srtp, #{srtp_worker := SRTP}=State) ->
    gen_statem:cast(SRTP, srtp),
    {keep_state_and_data, ?TIMEOUT};

listening(cast, Msg, State) ->
    ?Log(Msg, State),
    keep_state_and_data.


% Reason should be timeout or child_termination
terminate(Reason, StateName, #{dtls_worker := DTLS, srtp_worker := SRTP}=State) ->
    ?Log(Reason, StateName, State),
    ok = ?Log(dtls_worker_sup:terminate_child(DTLS)),
    ok = ?Log(srtp_worker_sup:terminate_child(SRTP)),
    ok.


%%====================================================================
%% Internal functions
%%====================================================================
