-module(vmq_rest_crl).

-behaviour(gen_server).

-define(ENV_API_KEYS, http_mgmt_api_keys).

%% API
-export([start_link/5,
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2]).

-record(state, {
        url,
        crlfile,
        timeout
        }).

-type state() :: #state{}.

start_link(Name, Url, CrlFile, Timeout, ApiKey) ->
    gen_server:start_link({local, Name}, ?MODULE, [Url, CrlFile, Timeout, ApiKey], []).

-spec init([]) -> {'ok', state()}.
init([Url, CrlFile, Timeout, ApiKey]) ->
    register_api_key(list_to_binary(ApiKey)),
    schedule_crl_poll_tick(Timeout),
    {ok, #state{url = Url, crlfile = CrlFile, timeout = Timeout}}.

-spec handle_info(_, _) -> {'noreply', _}.
handle_info(crl_poll_tick, #state{timeout = Timeout, url = Url, crlfile = CrlFile} = State) ->
    poll_crl(Url, CrlFile),
    schedule_crl_poll_tick(Timeout),
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
register_api_key(ApiKey) ->
    case ApiKey of 
        false -> error_logger:info_msg("No key to register");
        _ -> 
            Keys = vmq_config:get_env(vmq_server, ?ENV_API_KEYS, []),
            Keys1 = lists:delete(ApiKey, Keys),
            vmq_config:set_global_env(vmq_server, ?ENV_API_KEYS, [ApiKey|Keys1], true)
    end.

schedule_crl_poll_tick(Timeout) ->
    TickMS = Timeout,
    case TickMS of
        0 -> ok;
        _ ->
            erlang:send_after(TickMS, self(), crl_poll_tick)
    end.

poll_crl(Url, CrlFile) ->
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {Url, []}, [], []),
    [{_, Success}, {_, CRL}, _, _] = jsx:decode(unicode:characters_to_binary(Body)),
    PemHeader = unicode:characters_to_binary("-----BEGIN X509 CRL-----\n"),
    PemFooter = unicode:characters_to_binary("\n-----END X509 CRL-----"),
    case {Success, CRL} of 
        {false, _} -> error_logger:info_msg("API call failed");
        {true, undefined} -> error_logger:info_msg("could not parse CRL from API");
        {true, _} ->
            ok = file:write_file(CrlFile, [PemHeader, CRL, PemFooter])
    end.
