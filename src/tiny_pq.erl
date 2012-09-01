%% A priority queue based on gb_trees
-module(tiny_pq).

-export([
        delete_value/3, 
        insert_value/3, 
        move_value/4,

        foldr_new/4, 
        prune_old/2, 
        prune_collect_old/4
    ]).

%% @spec delete_value(Priority, Value, Tree) -> Tree1
%% @doc Delete a `Value' associated with `Priority' from `Tree'
delete_value(Priority, Value, Tree) ->
    case gb_trees:lookup(Priority, Tree) of
        {value, [Value]} ->
            gb_trees:delete(Priority, Tree);
        {value, Values} ->
            gb_trees:enter(Priority, lists:delete(Value, Values), Tree)
    end.

%% @spec insert_value(Priority, Value, Tree) -> Tree1
%% @doc Insert a `Value' with associated `Priority' into `Tree'
insert_value(Priority, Value, Tree) ->
    NewVal = case gb_trees:lookup(Priority, Tree) of
        none -> [Value];
        {value, ValueList} -> [Value|ValueList]
    end,
    gb_trees:enter(Priority, NewVal, Tree).

%% @spec move_value(OldPriority, NewPriority, Value, Tree) -> Tree1
%% @doc Change the priority of `Value' from `OldPriority' to `NewPriority'
move_value(OldPriority, NewPriority, Value, Tree) ->
    insert_value(NewPriority, Value, delete_value(OldPriority, Value, Tree)).

%% @spec foldr_new(Function, Acc0, Tree, Priority) -> Acc1
%% @doc Fold over values with priority greater than `Priority'
foldr_new(Function, Acc0, {_Size, TreeNode}, Priority) ->
    Acc1 = iterate_nonexpired_nodes(Function, Acc0, TreeNode, Priority),
    Acc1.

%% @spec prune_old(Tree, Priority) -> Tree1
%% @doc Remove nodes with priority less than or equal to `Priority'
prune_old({Size, TreeNode}, Priority) ->
    {Tree1, NumDeleted} = prune_expired_nodes(TreeNode, Priority),
    {Size - NumDeleted, Tree1}.

%% @spec prune_collect_old((Function, Acc0, Tree, Priority) -> {Acc1, Tree1}
%% @doc Remove nodes with priority less than or equal to `Priority', and
%% fold over them using `Function'
prune_collect_old(Function, Acc0, {Size, TreeNode}, Priority) ->
    {Acc1, Tree1, NumDeleted} = prune_collect_expired_nodes(Function, Acc0, TreeNode, Priority),
    {Acc1, {Size - NumDeleted, Tree1}}.


iterate_nonexpired_nodes(Function, State, {K, V, S, L}, Now) when K > Now ->
    Acc1 = iterate_nonexpired_nodes(Function, State, L, Now),
    Acc2 = lists:foldr(Function, Acc1, V),
    iterate_nonexpired_nodes(Function, Acc2, S, Now);
iterate_nonexpired_nodes(Function, State, {K, _V, _S, L}, Now) when K =< Now ->
    iterate_nonexpired_nodes(Function, State, L, Now);
iterate_nonexpired_nodes(_Function, State, nil, _Now) ->
    State.


prune_expired_nodes({K, V, S, L}, Now) when K > Now ->
    {Tree1, NumDeleted} = prune_expired_nodes(S, Now),
    {{K, V, Tree1, L}, NumDeleted};
prune_expired_nodes({K, _V, S, L}, Now) when K =< Now ->
    {_, NumDeleted_S} = prune_expired_nodes(S, Now),
    {Tree1, NumDeleted_L} = prune_expired_nodes(L, Now),
    {Tree1, NumDeleted_S + NumDeleted_L + 1};
prune_expired_nodes(nil, _Now) ->
    {nil, 0}.

prune_collect_expired_nodes(Function, Acc, {K, V, S, L}, Now) when K > Now ->
    {Acc1, Tree1, NumDeleted} = prune_collect_expired_nodes(Function, Acc, S, Now),
    {Acc1, {K, V, Tree1, L}, NumDeleted};
prune_collect_expired_nodes(Function, Acc, {K, V, S, L}, Now) when K =< Now ->
    Acc1 = lists:foldr(Function, Acc, V),
    {Acc2, _, NumDeleted_S} = prune_collect_expired_nodes(Function, Acc1, S, Now),
    {Acc3, Tree3, NumDeleted_L} = prune_collect_expired_nodes(Function, Acc2, L, Now),
    {Acc3, Tree3, NumDeleted_S + NumDeleted_L + 1};
prune_collect_expired_nodes(_Function, Acc, nil, _Now) ->
    {Acc, nil, 0}.
