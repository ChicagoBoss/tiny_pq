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

delete_value(Time, Value, Tree) ->
    case gb_trees:lookup(Time, Tree) of
        {value, [Value]} ->
            gb_trees:delete(Time, Tree);
        {value, Values} ->
            gb_trees:enter(Time, lists:delete(Value, Values), Tree)
    end.

insert_value(FutureTime, Value, Tree) ->
    NewVal = case gb_trees:lookup(FutureTime, Tree) of
        none -> [Value];
        {value, ValueList} -> [Value|ValueList]
    end,
    gb_trees:enter(FutureTime, NewVal, Tree).

move_value(OldTime, NewTime, Value, Tree) ->
    insert_value(NewTime, Value, delete_value(OldTime, Value, Tree)).

foldr_new(Function, State, {_Size, TreeNode}, Now) ->
    Acc1 = iterate_nonexpired_nodes(Function, State, TreeNode, Now),
    Acc1.

prune_old({Size, TreeNode}, Now) ->
    {Tree1, NumDeleted} = prune_expired_nodes(TreeNode, Now),
    {Size - NumDeleted, Tree1}.

prune_collect_old(Function, State, {Size, TreeNode}, Now) ->
    {Acc1, Tree1, NumDeleted} = prune_collect_expired_nodes(Function, State, TreeNode, Now),
    {Acc1, {Size - NumDeleted, Tree1}}.


iterate_nonexpired_nodes(Function, State, {K, V, S, L}, Now) when K > Now ->
    Acc1 = iterate_nonexpired_nodes(Function, State, L, Now),
    Acc2 = lists:foldr(Function, Acc1, V),
    iterate_nonexpired_nodes(Function, Acc2, S, Now);
iterate_nonexpired_nodes(Function, State, {K, V, S, L}, Now) when K =< Now ->
    iterate_nonexpired_nodes(Function, State, L, Now);
iterate_nonexpired_nodes(_Function, State, nil, _Now) ->
    State.


prune_expired_nodes({K, V, S, L}, Now) when K > Now ->
    {Tree1, NumDeleted} = prune_expired_nodes(S, Now),
    {{K, V, Tree1, L}, NumDeleted};
prune_expired_nodes({K, V, S, L}, Now) when K =< Now ->
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
