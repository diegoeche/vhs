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

handle_call({block_start, TapeName}, _From, {Impl, _S}) ->
  case file:consult(filename_(TapeName)) of
    {ok, [Calls]} ->
      Impl:block_start(Calls),
      {reply, ok, {Impl,Calls}};
    _ ->
      Impl:block_start([]),
      {reply, ok, {Impl, []}}
  end;

handle_call({block_end, TapeName}, _From, {Impl, Calls}) ->
  %% Make it configurable!
  file:write_file(filename_(TapeName), io_lib:fwrite("~p.\n", [Calls])),
  Impl:block_end(),
  {reply, ok, {Impl, []}}; %% Clean Server State

handle_call({record, Call}, _From, {Impl, S}) ->
  NewState = {Impl, [Call | S]},
  {reply, ok, NewState};

handle_call({server_state}, _From, S={_, Calls}) ->
  {reply, Calls, S}.

configure(ibrowse, _Options) ->
  start_link(vhs_ibrowse),
  vhs_ibrowse:configure(),
  ok;
configure(_, _) ->
  throw(adapter_not_supported).

use_cassette(TapeName, Block) ->
  block_start_(TapeName), %% Mounts mocks. Tries to load previous state from file with TapeName
  Block(),
  block_end_(TapeName).   %% Unmounts mocks. Saves the server state into TapeName

server_state() ->
  gen_server:call(?SERVER, {server_state}).

record(Call) ->
  gen_server:call(?SERVER, {record, Call}).

block_start_(TapeName) ->
  gen_server:call(?SERVER, {block_start, TapeName}).

block_end_(TapeName) ->
  gen_server:call(?SERVER, {block_end, TapeName}).

filename_(TapeName) ->
  "/tmp/" ++ atom_to_list(TapeName).
