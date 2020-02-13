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

-spec init([]) -> {'ok', {{'one_for_one', 5, 10},
                         [{atom(), {atom(), atom(), list()},
                           permanent, pos_integer(), worker, [atom()]}]}}.
init([]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(vmq_rest_crl, worker, [])]} }.
