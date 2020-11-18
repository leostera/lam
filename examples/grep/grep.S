{module, grep}.  %% version = 0

{exports, [{count,3},
           {count_all,2},
           {count_aux,2},
           {main,1},
           {module_info,0},
           {module_info,1}]}.

{attributes, []}.

{labels, 20}.


{function, count_aux, 2, 2}.
  {label,1}.
    {line,[{location,"grep.erl",10}]}.
    {func_info,{atom,grep},{atom,count_aux},2}.
  {label,2}.
    {move,{integer,0},{x,0}}.
    return.


{function, count, 3, 4}.
  {label,3}.
    {line,[{location,"grep.erl",13}]}.
    {func_info,{atom,grep},{atom,count},3}.
  {label,4}.
    {allocate,0,3}.
    {make_fun2,{f,18},0,0,3}.
    {line,[{location,"grep.erl",14}]}.
    {call_ext,1,{extfunc,erlang,spawn,1}}.
    {move,{atom,ok},{x,0}}.
    {deallocate,0}.
    return.


{function, count_all, 2, 6}.
  {label,5}.
    {line,[{location,"grep.erl",24}]}.
    {func_info,{atom,grep},{atom,count_all},2}.
  {label,6}.
    {test,is_eq_exact,{f,7},[{x,0},{integer,0}]}.
    {move,{x,1},{x,0}}.
    return.
  {label,7}.
    {allocate,2,2}.
    {move,{x,1},{y,0}}.
    {move,{x,0},{y,1}}.
    {move,{atom,infinity},{x,0}}.
    {line,[{location,"grep.erl",27}]}.
    {call_ext,1,{extfunc,process,recv,1}}.
    {test,is_tagged_tuple,{f,8},[{x,0},2,{atom,some}]}.
    {line,[{location,"grep.erl",28}]}.
    {gc_bif,'-',{f,0},1,[{y,1},{integer,1}],{x,1}}.
    {get_tuple_element,{x,0},1,{x,0}}.
    {line,[{location,"grep.erl",28}]}.
    {gc_bif,'+',{f,0},2,[{y,0},{x,0}],{x,0}}.
    {swap,{x,0},{x,1}}.
    {call_last,2,{f,6},2}.
  {label,8}.
    {line,[{location,"grep.erl",27}]}.
    {case_end,{x,0}}.


{function, main, 1, 10}.
  {label,9}.
    {line,[{location,"grep.erl",33}]}.
    {func_info,{atom,grep},{atom,main},1}.
  {label,10}.
    {test,is_nonempty_list,{f,9},[{x,0}]}.
    {allocate,2,1}.
    {get_list,{x,0},{y,1},{y,0}}.
    {bif,self,{f,0},[],{x,1}}.
    {move,{y,1},{x,0}}.
    {make_fun2,{f,16},0,0,2}.
    {'%',{var_info,{x,0},[{fun_type,{t_atom,[ok]}}]}}.
    {move,{y,0},{x,1}}.
    {line,[{location,"grep.erl",35}]}.
    {call_ext,2,{extfunc,lists,foreach,2}}.
    {line,[{location,"grep.erl",38}]}.
    {gc_bif,length,{f,0},0,[{y,0}],{x,0}}.
    {move,{integer,0},{x,1}}.
    {trim,1,1}.
    {line,[{location,"grep.erl",38}]}.
    {call,2,{f,6}}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{literal,<<"Found ~p occurrences of">>},{x,0}}.
    {line,[{location,"grep.erl",39}]}.
    {call_ext,2,{extfunc,io,format,2}}.
    {test_heap,2,0}.
    {put_list,{y,0},nil,{x,1}}.
    {move,{literal,<<"~p">>},{x,0}}.
    {line,[{location,"grep.erl",40}]}.
    {call_ext_last,2,{extfunc,io,format,2},1}.


{function, module_info, 0, 12}.
  {label,11}.
    {line,[]}.
    {func_info,{atom,grep},{atom,module_info},0}.
  {label,12}.
    {move,{atom,grep},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 14}.
  {label,13}.
    {line,[]}.
    {func_info,{atom,grep},{atom,module_info},1}.
  {label,14}.
    {move,{x,0},{x,1}}.
    {move,{atom,grep},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-main/1-fun-0-', 3, 16}.
  {label,15}.
    {line,[{location,"grep.erl",35}]}.
    {func_info,{atom,grep},{atom,'-main/1-fun-0-'},3}.
  {label,16}.
    {swap,{x,0},{x,2}}.
    {call_only,3,{f,4}}.


{function, '-count/3-fun-0-', 5, 18}.
  {label,17}.
    {line,[{location,"grep.erl",14}]}.
    {func_info,{atom,grep},{atom,'-count/3-fun-0-'},5}.
  {label,18}.
    {allocate,2,5}.
    {move,{x,3},{y,0}}.
    {move,{x,2},{y,1}}.
    {move,{x,4},{x,0}}.
    {line,[{location,"grep.erl",16}]}.
    {call_ext,1,{extfunc,file,read_file,1}}.
    {test,is_tagged_tuple,{f,19},[{x,0},2,{atom,ok}]}.
    {get_tuple_element,{x,0},1,{x,1}}.
    {move,{y,0},{x,0}}.
    {trim,1,1}.
    {line,[{location,"grep.erl",17}]}.
    {call,2,{f,2}}.
    {'%',{var_info,{x,0},[{type,{t_integer,{0,0}}}]}}.
    {move,{integer,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {line,[{location,"grep.erl",18}]}.
    {call_ext_last,2,{extfunc,erlang,send,2},1}.
  {label,19}.
    {line,[{location,"grep.erl",16}]}.
    {case_end,{x,0}}.
