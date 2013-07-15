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

test_how_it_works() ->
  ibrowse:start(),
  vhs:configure(ibrowse, []),
  vhs:use_cassette(doc_domain_test,
                   fun() ->
                       Response = ibrowse:send_req("http://www.iana.org/domains/reserved",
                                                   [],
                                                   get),

                       %% Uses the same structure of the mocked library.
                       {ok, Status, _Headers, Body} = Response,
                       ?assert_equal(Status, "200"),
                       ?assert(contains(Body, "Reserved Domains"))
                   end),
  ok.

test_an_error() ->
  ibrowse:start(),
  vhs:configure(ibrowse, []),
  vhs:use_cassette(error_test,
                   fun() ->
                       Response = ibrowse:send_req("http://www.iana.org/domains/example",
                                                   [],
                                                   get),

                       {ok, Status, _Headers, _Body} = Response,
                       ?assert_equal(Status, "500")
                   end),
  ok.
