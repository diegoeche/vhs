-module(vhs).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/1,
         stop/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% The real public interface %%
-export([configure/2,
         use_cassette/2,
         server_state/0,
         record/1]).

start_link(Impl) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Impl, []).

stop() ->
  gen_server:cast(?SERVER, stop).

init(Impl) -> {ok, {Impl, []}}.
handle_cast(stop, S) -> { stop, normal, S }.
handle_info(_, S) -> { noreply, S }.
terminate(_Reason, _S) -> ok.
code_change(_OldVsn, S, _Extra) -> { ok, S }.

%% handle_call %%
handle_call({configure_, ModuleName}, _From, S) ->
  {reply, ok, S};

handle_call({block_start, TapeName}, _From, {Impl, S}) ->
  Impl:block_start(),
  {reply, ok, {Impl,[]}}; %% Forget previous state.

handle_call({block_end, TapeName}, _From, S) ->
  {reply, ok, S};

handle_call({record, Call}, _From, {Impl, S}) ->
  NewState = {Impl, [Call | S]},
  {reply, ok, NewState};

handle_call({server_state}, _From, {_, S}) ->
  {reply, S, S}.

is_loc_key_blacklisted(Key) ->
  gen_server:call(?SERVER, {is_loc_key_blacklisted, Key}).

configure(ibrowse, Options) ->
  start_link(vhs_ibrowse),
  vhs_ibrowse:configure(),
  %...
  ok;
configure(_, _) ->
  throw(adapter_not_supported).

use_cassette(TapeName, Block) ->
  block_start_(TapeName), %% Mounts mocks. Tries to load previous state from file with TapeName
  Block(),
  block_end_(TapeName).   %% Unmounts mocks. Saves the server state into TapeName

play_cassette(TapeName) ->
  gen_server:call(?SERVER, {play_cassette}).

server_state() ->
  gen_server:call(?SERVER, {server_state}).

record(Call) ->
  gen_server:call(?SERVER, {record, Call}).

block_start_(TapeName) ->
  gen_server:call(?SERVER, {block_start, TapeName}).

block_end_(TapeName) ->
  gen_server:call(?SERVER, {block_end, TapeName}).

handle_mocked_request() ->
  ok.
