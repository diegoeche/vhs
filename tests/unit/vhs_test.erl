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
test_use_cassette_with_ibrowse_adapter() ->
  ibrowse:start(),

  vhs:configure(ibrowse, []),
  vhs:use_cassette(iana_domain_test,
                   fun() ->
                       ibrowse:send_req("http://www.iana.org/domains/example/", [], get),
                       Calls = [{Request, Response}] = vhs:server_state(),
                       ?assert_equal(Request, ["http://www.iana.org/domains/example/", [], get])
                   end),

  %% Cleans the state of the server after the block is executed
  ?assert_equal([], vhs:server_state()),

  %% It should have the nice side-effect of creating a new file
  {ok, [StoredCalls]} = file:consult("/tmp/iana_domain_test"),

  %% The number of stored calls should correspond to the calls done inside of the block.
  ?assert_equal(1, length(StoredCalls)),
  ok.

