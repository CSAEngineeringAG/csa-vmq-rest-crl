%%%-------------------------------------------------------------------
%% @doc vmq_rest_crl top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(vmq_rest_crl_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, CrlConfigs} = application:get_env(vmq_rest_crl, endpoints),
    Children = [get_children(H) || H <- CrlConfigs],
    {ok, { {one_for_one, 5, 10}, Children } }.

get_children({_, Map}) ->
    [CrlFile, Name, Refresh, Url] = maps:values(Map),
    {{vmq_rest_crl, Name},
                 {vmq_rest_crl, start_link, [Name, Url, CrlFile, Refresh]},
                 permanent, 5000, worker, [vmq_rest_crl]}.