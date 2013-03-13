-module (doc_test).
-include_lib ("etest/include/etest.hrl").
-include_lib ("etest_http/include/etest_http.hrl").
-compile (export_all).

contains(Text, Part) ->
  {BinaryText, BinaryPart} = {list_to_binary(Text), list_to_binary(Part)},
  case binary:match(BinaryText, BinaryPart) of
    nomatch -> false;
    _ -> true
  end.

%% vhs:use_cassete should save all the request-responses into the tape file
test_how_it_works() ->
  ibrowse:start(),
  vhs:configure(ibrowse, []),
  vhs:use_cassette(doc_domain_test,
                   fun() ->
                       Response = ibrowse:send_req("http://www.iana.org/domains/example",
                                                   [],
                                                   get),

                       %% Uses the same structure of the mocked library.
                       {ok, Status, _Headers, Body} = Response,
                       ?assert_equal(Status, "200"),
                       ?assert(contains(Body, "Example Domain"))
                   end),
  ok.
