-module(vmq_rest_crl).

-behaviour(gen_server).

%% API
-export([start_link/0,
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2]).

-record(state, {}).
-type state() :: #state{}.

-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init([]) -> {'ok', state()}.
init([]) ->
    schedule_crl_poll_tick(),
    {ok, #state{}}.

-spec handle_info(_, _) -> {'noreply', _}.
handle_info(crl_poll_tick, State) ->
    poll_crl(),
    schedule_crl_poll_tick(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

-spec handle_cast(_, _) -> {'noreply', _}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_call(_, _, _) -> {'noreply', _}.
handle_call(_Msg, _From, State) ->
    {noreply, State}.

%% Internal functions


schedule_crl_poll_tick() ->
    TickMS = 10000,%%vmq_config:get_env(crl_refresh_interval, 60000),
    case TickMS of
        0 -> ok;
        _ ->
            erlang:send_after(TickMS, self(), crl_poll_tick)
    end.

poll_crl() ->
    {ok, CrlConfigs} = application:get_env(vmq_rest_crl, endpoints),
    {_, Map} = lists:nth(1, CrlConfigs),
    [CrlFile, _, _, Url] = maps:values(Map),
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {Url, []}, [], []),
    [{_, Success}, {_, CRL}, _, _] = jsx:decode(unicode:characters_to_binary(Body)),
    case {Success, CRL} of 
        {false, _} -> error_logger:info_msg("API call failed");
        {true, undefined} -> error_logger:info_msg("could not parse CRL from API");
        {true, _} ->
            io:format("success"),
            ok = file:write_file(CrlFile, CRL)
    end.
