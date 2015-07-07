%%%---------------------------------------------------------------------------
%%% @doc PropList helper functions
%%% @end
%%%---------------------------------------------------------------------------

-module(xproplists).

-export([delete_all/2,
         partition/2,
         take_value/3,
         take_values/2]).

-type proplist() :: list({atom(), any()}).

%% Delete all the instances of the keys provided.
-spec delete_all(Keys :: list(atom()),
                 List :: proplist()) -> proplist().

delete_all([], List) ->
    List;

delete_all([Key | Keys], List)
    when is_atom(Key), is_list(List) ->
    delete_all(Keys, proplists:delete(Key, List)).

%% Partition the proplist based on a key.
-spec partition(Key :: atom(),
                List :: proplist()) -> {proplist(), proplist()}.

partition(Key, List)
    when is_atom(Key), is_list(List) ->
    lists:partition(fun({K, _}) -> K == Key end, List).

%% Remove a key from the proplist.
-spec take_value(Key :: atom(),
                 List :: proplist(),
                 Default :: any()) -> {any(), proplist()}.

take_value(Key, List, Default)
    when is_atom(Key), is_list(List) ->
    case lists:keytake(Key, 1, List) of
        false ->
            {Default, List};
        {value, {Key, Value}, RemainingList} ->
            {Value, RemainingList}
    end.

%% Remove many keys from the proplist.
-spec take_values(DefaultList :: proplist(),
                  List :: proplist()) -> list().

take_values(DefaultList, List)
    when is_list(DefaultList), is_list(List) ->
    take_values([], DefaultList, List).

take_values(Result, [], List)
    when is_list(Result), is_list(List) ->
    lists:reverse(Result) ++ List;

take_values(Result, [{Key, Default} | DefaultList], List)
    when is_list(Result), is_atom(Key), is_list(List) ->
    case lists:keytake(Key, 1, List) of
        false ->
            take_values([Default | Result], DefaultList, List);
        {value, {Key, Value}, RemainingList} ->
            take_values([Value | Result], DefaultList, RemainingList)
    end.