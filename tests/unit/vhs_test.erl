-module (vhs_test).
-include_lib ("etest/include/etest.hrl").
-compile (export_all).

%% vhs:configure should fail with a non-supported adapter
test_configure_with_unsupported_adapter() ->
  ?assert_throw(adapter_not_supported,
                vhs:configure(adapter_not_supported, [])).
