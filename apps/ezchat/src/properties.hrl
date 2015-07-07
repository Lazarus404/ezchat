
-export([display_port/0, display_domain/0, display_secure/0, ezchat_domain/0, ezchat_port/0, is_secure/0, secure_cert/0, secure_ca_cert/0, secure_key/0, cluster_node/1]).

-define(DEFAULT_PORT, 8080).
-define(DEFAULT_XHTTP_DOMAIN, <<"127.0.0.1">>).
-define(DEFAULT_TURN_DOMAIN, <<"127.0.0.1">>).
-define(DEFAULT_TURN_PATH, <<"">>).
-define(DEFAULT_TURN_PORT, 80).
-define(DEFAULT_TURN_PORT_SECURE, 443).
-define(DEFAULT_SECURE_CA_CERT, undefined).
-define(DEFAULT_SECURE_CERT, undefined).
-define(DEFAULT_SECURE_KEY, undefined).

display_domain() ->
	case init:get_argument(display_domain) of
		{ok, [[D]]} ->
			list_to_binary(D);
		_ ->
			ezchat_server:ezchat_domain()
	end.

display_secure() ->
	case ezchat_server:is_secure() of
		true -> true;
		_ ->
			case init:get_argument(display_secure) of
				{ok, [[D]]} ->
					list_to_atom(D);
				_ ->
					false
			end
	end.

display_port() ->
	case init:get_argument(display_port) of
		{ok, [[P]]} -> case is_numeric(P) and is_integer(list_to_integer(P)) of
			true -> list_to_integer(P);
			_ -> ezchat_server:ezchat_port()
		end;
		_ -> ezchat_server:ezchat_port()
	end.

ezchat_domain() ->
	case init:get_argument(ezchat_domain) of
		{ok, [[D]]} ->
			list_to_binary(D);
		_ ->
			?DEFAULT_XHTTP_DOMAIN
	end.

ezchat_port() ->
	case init:get_argument(port) of
		{ok, [[P]]} -> case is_numeric(P) and is_integer(list_to_integer(P)) of
			true -> list_to_integer(P);
			_ -> ?DEFAULT_PORT
		end;
		_ -> ?DEFAULT_PORT
	end.

is_secure() ->
	case init:get_argument(is_secure) of
		{ok, [[D]]} ->
			list_to_atom(D);
		_ ->
			false
	end.

secure_ca_cert() ->
	case init:get_argument(secure_ca_cert) of
		{ok, [[D]]} ->
			list_to_binary(D);
		_ ->
			?DEFAULT_SECURE_CA_CERT
	end.

secure_cert() ->
	case init:get_argument(secure_cert) of
		{ok, [[D]]} ->
			list_to_binary(D);
		_ ->
			?DEFAULT_SECURE_CERT
	end.

secure_key() ->
	case init:get_argument(secure_key) of
		{ok, [[D]]} ->
			list_to_binary(D);
		_ ->
			?DEFAULT_SECURE_KEY
	end.

cluster_node(Arg) ->
	case init:get_argument(Arg) of
		{ok, [[P]]} ->
			[list_to_atom(P)];
		_ ->
			case ezchat_utils:get_env(ezchat, cluster, []) of
				L when is_list(L), L =/= [] ->
					L;
				_ ->
					undefined
			end
	end.