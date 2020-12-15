{module, lists}.  %% version = 0

{exports, [{foldl,3},
           {foreach,2},
           {main,1},
           {map,2},
           {module_info,0},
           {module_info,1}]}.

{attributes, []}.

{labels, 18}.


{function, map, 2, 2}.
  {label,1}.
    {line,[{location,"lists.erl",32}]}.
    {func_info,{atom,lists},{atom,map},2}.
  {label,2}.
    {test,is_nonempty_list,{f,3},[{x,1}]}.
    {allocate,2,2}.
    {move,{x,0},{y,1}}.
    {get_list,{x,1},{x,0},{y,0}}.
    {move,{y,1},{x,1}}.
    {line,[{location,"lists.erl",33}]}.
    {call_fun,1}.
    {move,{y,0},{x,2}}.
    {swap,{y,1},{x,0}}.
    {move,{x,2},{x,1}}.
    {trim,1,1}.
    {line,[{location,"lists.erl",33}]}.
    {call,2,{f,2}}.
    {'%',{var_info,{x,0},[{type,{t_list,any,nil}}]}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,3}.
    {test,is_nil,{f,1},[{x,1}]}.
    {test,is_function2,{f,1},[{x,0},{integer,1}]}.
    {move,nil,{x,0}}.
    return.


{function, foreach, 2, 5}.
  {label,4}.
    {line,[{location,"lists.erl",41}]}.
    {func_info,{atom,lists},{atom,foreach},2}.
  {label,5}.
    {test,is_nonempty_list,{f,6},[{x,1}]}.
    {allocate,2,2}.
    {move,{x,0},{y,1}}.
    {get_list,{x,1},{x,0},{y,0}}.
    {move,{y,1},{x,1}}.
    {line,[{location,"lists.erl",42}]}.
    {call_fun,1}.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,0}}.
    {call_last,2,{f,5},2}.
  {label,6}.
    {test,is_nil,{f,4},[{x,1}]}.
    {move,{atom,ok},{x,0}}.
    return.


{function, foldl, 3, 8}.
  {label,7}.
    {line,[{location,"lists.erl",55}]}.
    {func_info,{atom,lists},{atom,foldl},3}.
  {label,8}.
    {test,is_nonempty_list,{f,9},[{x,2}]}.
    {allocate,2,3}.
    {move,{x,0},{y,1}}.
    {get_list,{x,2},{x,0},{y,0}}.
    {move,{y,1},{x,2}}.
    {line,[{location,"lists.erl",55}]}.
    {call_fun,2}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,2}}.
    {move,{y,1},{x,0}}.
    {call_last,3,{f,8},2}.
  {label,9}.
    {test,is_nil,{f,7},[{x,2}]}.
    {test,is_function2,{f,7},[{x,0},{integer,2}]}.
    {move,{x,1},{x,0}}.
    return.


{function, main, 1, 11}.
  {label,10}.
    {line,[{location,"lists.erl",58}]}.
    {func_info,{atom,lists},{atom,main},1}.
  {label,11}.
    {allocate,0,1}.
    {make_fun2,{f,17},0,0,1}.
    {'%',{var_info,{x,0},[{fun_type,number}]}}.
    {move,{literal,[1,2,3]},{x,2}}.
    {move,{integer,0},{x,1}}.
    {line,[{location,"lists.erl",59}]}.
    {call_ext,3,{extfunc,lists,foldl,3}}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{literal,<<"~p\n">>},{x,0}}.
    {line,[{location,"lists.erl",63}]}.
    {call_ext_last,2,{extfunc,io,format,2},0}.


{function, module_info, 0, 13}.
  {label,12}.
    {line,[]}.
    {func_info,{atom,lists},{atom,module_info},0}.
  {label,13}.
    {move,{atom,lists},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 15}.
  {label,14}.
    {line,[]}.
    {func_info,{atom,lists},{atom,module_info},1}.
  {label,15}.
    {move,{x,0},{x,1}}.
    {move,{atom,lists},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-main/1-fun-0-', 3, 17}.
  {label,16}.
    {line,[{location,"lists.erl",59}]}.
    {func_info,{atom,lists},{atom,'-main/1-fun-0-'},3}.
  {label,17}.
    {allocate_heap,2,2,3}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{y,1}}.
    {put_list,{x,2},nil,{x,1}}.
    {move,{literal,"~p"},{x,0}}.
    {line,[{location,"lists.erl",60}]}.
    {call_ext,2,{extfunc,io,format,2}}.
    {line,[{location,"lists.erl",61}]}.
    {gc_bif,'+',{f,0},0,[{y,0},{y,1}],{x,0}}.
    {deallocate,2}.
    return.
