-module (db_extra).
-export ([ limit/4, select/4 ]).

select(Tab, Field, Offset, Number) ->
    {atomic, Ret} = mnesia:transaction(fun()-> limit(Tab, Field, Offset, Number) end),
    Ret.

limit (Tab, Field, Offset, Number) ->
  limit (get_field_number (Tab, Field),
         Offset,
         Number,
         mnesia:select (Tab,
                        [ { mnesia:table_info (Tab, wild_pattern), [], [ '$_' ] } ],
                        10,
                        read),
         gb_sets:empty ()).

get_offset ([ Field | _ ], Field, N) -> N;
get_offset ([ _ | T ], Field, N) -> get_offset (T, Field, N + 1).

get_field_number (Tab, Field) ->
  get_offset (mnesia:table_info (Tab, attributes), Field, 2).

limit (_FieldNumber, Offset, Number, '$end_of_table', Tree) ->
  Candidates = [ Record || { _, Record } <- gb_sets:to_list (Tree) ],
  { _, Rest } = safe_split (Offset, Candidates),
  { Result, _ } = safe_split (Number, Rest),
  Result;
limit (FieldNumber, Offset, Number, { Results, Cont }, Tree) ->
  NewTree =
    lists:foldl (fun (Record, AccTree) ->
                   Key = { element (FieldNumber, Record), Record },
                   gb_sets:add (Key, AccTree)
                 end,
                 Tree,
                 Results),

  PrunedTree = prune_tree (NewTree, Offset + Number),

  limit (FieldNumber,
         Offset,
         Number,
         mnesia:select (Cont),
         PrunedTree).

prune_tree (Tree, Max) ->
  case gb_sets:size (Tree) > Max of
    true ->
      { _, NewTree } = gb_sets:take_largest (Tree),
      prune_tree (NewTree, Max);
    false ->
      Tree
  end.

safe_split (N, L) when length (L) >= N ->
  lists:split (N, L);
safe_split (_N, L) ->
  { L, [] }.