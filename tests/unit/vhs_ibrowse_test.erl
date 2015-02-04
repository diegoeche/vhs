-module (vhs_ibrowse_test).
-include_lib ("etest/include/etest.hrl").
-compile (export_all).

%% vhs:configure should work for ibrowse
test_configure_with_ibrowse_adapter() ->
  ?assert_no_throw(adapter_not_supported,
                   vhs:configure(ibrowse, [])).

%% vhs:use_cassete should save all the request-responses into the tape file
xtest_recording_a_call_with_ibrowse_adapter() ->
    ibrowse:start(),
    vhs:configure(ibrowse, []),
    AssertHeader = fun(Key, Headers) ->
                           ?assert(proplists:is_defined(Key, Headers))
                   end,
    AssertionFun = fun() ->
                           ibrowse:send_req("http://localhost:8000/200.html", [], get),
                           [{Request, Response}] = vhs:server_state(),
                           ExpectedRequest = ["http://localhost:8000/200.html", [], get],
                           ?assert_equal(ExpectedRequest, Request),
                           {ok, Status, Headers, _} = Response,
                           ?assert_equal("200", Status),
                           ExpectedHeaders = ["Date", "Server", "Content-Length",  "Content-type"],
                           [ AssertHeader(Key, Headers) || Key <- ExpectedHeaders ]
                   end,
    vhs:use_cassette(iana_domain_test, AssertionFun),

    %% Cleans the state of the server after the block is executed
    ?assert_equal([], vhs:server_state()),

    %% It should have the nice side-effect of creating a new file
    {ok, [StoredCalls]} = file:consult("/tmp/iana_domain_test"),

    %% The number of stored calls should correspond to the calls done inside of the block.
    ?assert_equal(1, length(StoredCalls)).

%% vhs:use_cassete should save all the request-responses into the tape file
test_invariants_when_no_call_is_performed() ->
  ibrowse:start(),
  vhs:configure(ibrowse, []),
  vhs:use_cassette(another_call,
                   fun() ->
                       ?assert_equal([], vhs:server_state())
                   end),

  %% Cleans the state of the server after the block is executed
  ?assert_equal([], vhs:server_state()),

  %% It should have the nice side-effect of creating a new file
  {ok, [[]]} = file:consult("/tmp/another_call"),
  ok.
