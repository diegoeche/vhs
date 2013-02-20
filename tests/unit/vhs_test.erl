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

%% vhs:use_casset should save all the request-responses into the tape file
test_use_cassette_with_ibrowse_adapter() ->
  ibrowse:start(),

  vhs:configure(ibrowse, []),
  vhs:use_cassette(iana_domain_test,
                   fun() ->
                       case ibrowse:send_req("http://www.iana.org/domains/example/",
                                             [],
                                             get) of
                         Response ->
                           io:format("Body: ~p ~n", [Response]);
                         Other -> io:format("Body: ~p ~n", [Other])
                       end
                   end),

  Calls = vhs:server_state(),
  io:format("Calls: ~p~n", [Calls]),
  ok.
