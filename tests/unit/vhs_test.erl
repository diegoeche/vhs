-module (vhs_test).
-include_lib ("etest/include/etest.hrl").
-include_lib ("etest_http/include/etest_http.hrl").
-compile (export_all).

%% vhs:configure should fail with a non-supported adapter
test_configure_with_unsupported_adapter() ->
  ?assert_throw(adapter_not_supported,
                vhs:configure(adapter_not_supported, [])).

%% vhs:configure should work for ibrowse
test_configure_with_ibrowse_adapter() ->
  ?assert_no_throw(adapter_not_supported,
                   vhs:configure(ibrowse, [])).

%% vhs:use_cassete should save all the request-responses into the tape file
test_recording_a_call_with_ibrowse_adapter() ->
  ibrowse:start(),
  vhs:configure(ibrowse, []),
  vhs:use_cassette(iana_domain_test,
                   fun() ->
                       ibrowse:send_req("http://www.iana.org/domains/example/", [], get),
                       [{Request, Response}] = vhs:server_state(),
                       ?assert_equal(Request, ["http://www.iana.org/domains/example/", [], get]),
                       ?assert_equal(Response, {ok,"302",
                                                [{"Date","Wed, 13 Mar 2013 09:48:03 GMT"},
                                                 {"Server","Apache/2.2.3 (CentOS)"},
                                                 {"Location","http://www.iana.org/domains/example"},
                                                 {"Content-Length","0"},
                                                 {"Connection","close"},
                                                 {"Content-Type","text/html; charset=utf-8"}],
                                                []})
                   end),

  %% Cleans the state of the server after the block is executed
  ?assert_equal([], vhs:server_state()),

  %% It should have the nice side-effect of creating a new file
  {ok, [StoredCalls]} = file:consult("/tmp/iana_domain_test"),

  %% The number of stored calls should correspond to the calls done inside of the block.
  ?assert_equal(1, length(StoredCalls)),
  ok.

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
