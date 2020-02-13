%%%-------------------------------------------------------------------
%% @doc vmq_rest_crl public API
%% @end
%%%-------------------------------------------------------------------

-module(vmq_rest_crl_app).

-behaviour(application).

-export([start/2, 
		stop/1]).

start(_StartType, _StartArgs) ->
    vmq_rest_crl_sup:start_link().

-spec stop(_) -> 'ok'.
stop(_State) ->
    ok.
