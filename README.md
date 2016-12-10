# VHS

VHS is a webmocking framework for Erlang inspired by [Ruby's vcr
gem](https://github.com/vcr/vcr). In the moment it only supports the
[ibrowse http client](https://github.com/cmullaparthi/ibrowse), but support for other
libraries should be easy to add.

# Show me the code

```erlang
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

```

# How does it work?

VHS uses Meck to wrap ibrowse's `send_req` function and check whether the request has
been performed already. The first time, vhs will record the request-response into a
file, for later calls.

# Tests

The tests are using in [etest](https://github.com/wooga/etest). In order to run the tests locally
you just have to run the following command:

```
./bin/test
```

This will spawn a static server, compile the code and run the tests.

# Contributing

Pull Request. And always add a test to account for the behavior change/Feature.
