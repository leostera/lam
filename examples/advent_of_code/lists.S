{module, lists}.  %% version = 0

{exports, [{foldl,3},{foreach,2},{main,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 15}.


{function, foreach, 2, 2}.
  {label,1}.
    {line,[{location,"lists.erl",30}]}.
    {func_info,{atom,lists},{atom,foreach},2}.
  {label,2}.
    {test,is_nonempty_list,{f,3},[{x,1}]}.
    {allocate,2,2}.
    {move,{x,0},{y,1}}.
    {get_list,{x,1},{x,0},{y,0}}.
    {move,{y,1},{x,1}}.
    {line,[{location,"lists.erl",31}]}.
    {call_fun,1}.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,0}}.
    {call_last,2,{f,2},2}.
  {label,3}.
    {test,is_nil,{f,1},[{x,1}]}.
    {move,{atom,ok},{x,0}}.
    return.


{function, foldl, 3, 5}.
  {label,4}.
    {line,[{location,"lists.erl",44}]}.
    {func_info,{atom,lists},{atom,foldl},3}.
  {label,5}.
    {test,is_nonempty_list,{f,6},[{x,2}]}.
    {allocate,2,3}.
    {move,{x,0},{y,1}}.
    {get_list,{x,2},{x,0},{y,0}}.
    {move,{y,1},{x,2}}.
    {line,[{location,"lists.erl",44}]}.
    {call_fun,2}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,2}}.
    {move,{y,1},{x,0}}.
    {call_last,3,{f,5},2}.
  {label,6}.
    {test,is_nil,{f,4},[{x,2}]}.
    {test,is_function2,{f,4},[{x,0},{integer,2}]}.
    {move,{x,1},{x,0}}.
    return.


{function, main, 1, 8}.
  {label,7}.
    {line,[{location,"lists.erl",47}]}.
    {func_info,{atom,lists},{atom,main},1}.
  {label,8}.
    {allocate,0,1}.
    {make_fun2,{f,14},0,0,1}.
    {'%',{var_info,{x,0},[{fun_type,number}]}}.
    {move,{literal,[1,2,3]},{x,2}}.
    {move,{integer,0},{x,1}}.
    {line,[{location,"lists.erl",48}]}.
    {call_ext,3,{extfunc,lists,foldl,3}}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{literal,<<"~p\n">>},{x,0}}.
    {line,[{location,"lists.erl",52}]}.
    {call_ext_last,2,{extfunc,io,format,2},0}.


{function, module_info, 0, 10}.
  {label,9}.
    {line,[]}.
    {func_info,{atom,lists},{atom,module_info},0}.
  {label,10}.
    {move,{atom,lists},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 12}.
  {label,11}.
    {line,[]}.
    {func_info,{atom,lists},{atom,module_info},1}.
  {label,12}.
    {move,{x,0},{x,1}}.
    {move,{atom,lists},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-main/1-fun-0-', 3, 14}.
  {label,13}.
    {line,[{location,"lists.erl",48}]}.
    {func_info,{atom,lists},{atom,'-main/1-fun-0-'},3}.
  {label,14}.
    {allocate_heap,2,2,3}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{y,1}}.
    {put_list,{x,2},nil,{x,1}}.
    {move,{literal,"~p"},{x,0}}.
    {line,[{location,"lists.erl",49}]}.
    {call_ext,2,{extfunc,io,format,2}}.
    {line,[{location,"lists.erl",50}]}.
    {gc_bif,'+',{f,0},0,[{y,0},{y,1}],{x,0}}.
    {deallocate,2}.
    return.
