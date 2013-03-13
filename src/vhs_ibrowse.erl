-module(vhs_ibrowse).
-export([configure/1,
         configure/0,
         block_end/0,
         block_start/1]).

configure(_Opts) ->
  %% io:format("Starting!~n", []),
  try meck:new(ibrowse, [passthrough])
  catch
    error:_ -> ok
  end.

configure() ->
  configure([]).

block_start(AllCalls) ->
  %% Unsure whether this code is really specific for ibrowse or could be lifted
  %% into the main vhs module
  MockBehavior = fun(Args) ->
                     case proplists:get_value(Args, AllCalls) of
                       undefined ->
                         Response = meck:passthrough(Args),
                         Call = {Args, Response},
                         vhs:record(Call),
                         Response;
                       Response -> Response
                     end
                 end,
  mock_3_to_6_arg_function_(ibrowse, send_req, MockBehavior).

block_end() ->
  meck:unload(ibrowse).

%% HACK
mock_3_to_6_arg_function_(Module, FuncName, MockBehavior) ->
  meck:expect(Module, FuncName, fun(A1,A2,A3) -> MockBehavior([A1,A2,A3]) end),
  meck:expect(Module, FuncName, fun(A1,A2,A3,A4) -> MockBehavior([A1,A2,A3,A4]) end),
  meck:expect(Module, FuncName, fun(A1,A2,A3,A4,A5) -> MockBehavior([A1,A2,A3,A4,A5]) end),
  meck:expect(Module, FuncName, fun(A1,A2,A3,A4,A5,A6) -> MockBehavior([A1,A2,A3,A4,A5,A6]) end),
  ok.
