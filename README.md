# VHS #

VHS is a webmocking framework for Erlang inspired by (Ruby's vcr
gem)[https://github.com/vcr/vcr]. In the moment it only supports the
(ibrowse http client)[https://github.com/cmullaparthi/ibrowse], but support for other
libraries should be easy to add.

# Show me the Code #

```erlang


```

# How does it work? #

VHS uses Meck to wrap ibrowse's `send_req` function and check whether the request has
been performed already. The first time, vhs will record the request-response into a
file, for later calls.

# Contributing #

Pull Request. And always add a test to account for the behavior change/Feature.
