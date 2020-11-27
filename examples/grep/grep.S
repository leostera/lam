{module, grep}.  %% version = 0

{exports, [{main,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 24}.


{function, print_matches, 3, 2}.
  {label,1}.
    {line,[{location,"grep.erl",6}]}.
    {func_info,{atom,grep},{atom,print_matches},3}.
  {label,2}.
    {test,is_nonempty_list,{f,4},[{x,1}]}.
    {get_list,{x,1},{x,3},{x,1}}.
    {test,is_eq_exact,{f,3},[{x,3},{x,0}]}.
    {allocate_heap,3,6,3}.
    {move,{x,1},{y,0}}.
    {move,{x,2},{y,1}}.
    {move,{x,0},{y,2}}.
    {bif,self,{f,0},[],{x,0}}.
    {put_list,{y,2},nil,{x,1}}.
    {put_list,{x,2},{x,1},{x,1}}.
    {put_list,{x,0},{x,1},{x,1}}.
    {move,{literal,<<"~p | ~p: ~p\n">>},{x,0}}.
    {line,[{location,"grep.erl",7}]}.
    {call_ext,2,{extfunc,io,format,2}}.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,2}}.
    {move,{y,2},{x,0}}.
    {call_last,3,{f,2},3}.
  {label,3}.
    {call_only,3,{f,2}}.
  {label,4}.
    {move,{atom,ok},{x,0}}.
    return.


{function, count, 3, 6}.
  {label,5}.
    {line,[{location,"grep.erl",13}]}.
    {func_info,{atom,grep},{atom,count},3}.
  {label,6}.
    {make_fun2,{f,22},0,0,3}.
    {line,[{location,"grep.erl",14}]}.
    {call_ext_only,1,{extfunc,erlang,spawn,1}}.


{function, count_all, 1, 8}.
  {label,7}.
    {line,[{location,"grep.erl",22}]}.
    {func_info,{atom,grep},{atom,count_all},1}.
  {label,8}.
    {'%',{var_info,{x,0},[{type,{t_list,any,nil}}]}}.
    {test,is_nonempty_list,{f,12},[{x,0}]}.
    {allocate,1,1}.
    {get_tl,{x,0},{y,0}}.
  {label,9}.
    {loop_rec,{f,11},{x,0}}.
    {test,is_eq_exact,{f,10},[{x,0},{atom,done}]}.
    remove_message.
    {move,{y,0},{x,0}}.
    {call_last,1,{f,8},1}.
  {label,10}.
    {loop_rec_end,{f,9}}.
  {label,11}.
    {wait,{f,9}}.
  {label,12}.
    {move,{atom,ok},{x,0}}.
    return.


{function, main, 1, 14}.
  {label,13}.
    {line,[{location,"grep.erl",25}]}.
    {func_info,{atom,grep},{atom,main},1}.
  {label,14}.
    {test,is_nonempty_list,{f,13},[{x,0}]}.
    {allocate_zero,2,1}.
    {get_list,{x,0},{x,0},{y,1}}.
    {bif,self,{f,0},[],{x,1}}.
    {make_fun2,{f,20},0,0,2}.
    {line,[{location,"grep.erl",28}]}.
    {gc_bif,length,{f,0},1,[{y,1}],{x,1}}.
    {test_heap,2,2}.
    {put_list,{x,1},nil,{x,1}}.
    {move,{x,0},{y,0}}.
    {move,{literal,<<"About to check ~p files\n">>},{x,0}}.
    {line,[{location,"grep.erl",28}]}.
    {call_ext,2,{extfunc,io,format,2}}.
    {move,{y,1},{x,1}}.
    {move,{y,0},{x,0}}.
    {trim,1,1}.
    {line,[{location,"grep.erl",29}]}.
    {call_ext,2,{extfunc,lists,foreach,2}}.
    {line,[{location,"grep.erl",30}]}.
    {gc_bif,length,{f,0},0,[{y,0}],{x,0}}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{literal,<<"Spawned ~p processes...awaiting replies.\n">>},{x,0}}.
    {line,[{location,"grep.erl",30}]}.
    {call_ext,2,{extfunc,io,format,2}}.
    {move,{y,0},{x,0}}.
    {call_last,1,{f,8},1}.


{function, module_info, 0, 16}.
  {label,15}.
    {line,[]}.
    {func_info,{atom,grep},{atom,module_info},0}.
  {label,16}.
    {move,{atom,grep},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 18}.
  {label,17}.
    {line,[]}.
    {func_info,{atom,grep},{atom,module_info},1}.
  {label,18}.
    {move,{x,0},{x,1}}.
    {move,{atom,grep},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-main/1-fun-0-', 3, 20}.
  {label,19}.
    {line,[{location,"grep.erl",27}]}.
    {func_info,{atom,grep},{atom,'-main/1-fun-0-'},3}.
  {label,20}.
    {allocate,2,3}.
    {move,{x,2},{y,0}}.
    {move,{x,0},{y,1}}.
    {move,{x,1},{x,0}}.
    {line,[{location,"grep.erl",27}]}.
    {call_ext,1,{extfunc,binary,list_to_bin,1}}.
    {move,{y,1},{x,1}}.
    {move,{x,0},{x,2}}.
    {move,{y,0},{x,0}}.
    {call_last,3,{f,6},2}.


{function, '-count/3-fun-0-', 3, 22}.
  {label,21}.
    {line,[{location,"grep.erl",14}]}.
    {func_info,{atom,grep},{atom,'-count/3-fun-0-'},3}.
  {label,22}.
    {allocate,4,3}.
    {init,{y,0}}.
    {move,{x,2},{y,1}}.
    {move,{x,1},{y,2}}.
    {move,{x,0},{y,3}}.
    {move,{x,1},{x,0}}.
    {line,[{location,"grep.erl",15}]}.
    {call_ext,1,{extfunc,file,read_file,1}}.
    {test,is_tagged_tuple,{f,23},[{x,0},2,{atom,ok}]}.
    {get_tuple_element,{x,0},1,{x,0}}.
    {move,{literal,[global]},{x,2}}.
    {move,{literal,[<<" ">>,<<"\n">>]},{x,1}}.
    {line,[{location,"grep.erl",16}]}.
    {call_ext,3,{extfunc,binary,split,3}}.
    {test_heap,4,1}.
    {put_list,{x,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {move,{x,0},{y,0}}.
    {move,{literal,"~p ~p\n"},{x,0}}.
    {line,[{location,"grep.erl",17}]}.
    {call_ext,2,{extfunc,io,format,2}}.
    {move,{y,0},{x,1}}.
    {move,{y,2},{x,2}}.
    {move,{y,1},{x,0}}.
    {trim,3,1}.
    {line,[{location,"grep.erl",18}]}.
    {call,3,{f,2}}.
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {move,{atom,done},{x,1}}.
    {move,{y,0},{x,0}}.
    {line,[{location,"grep.erl",19}]}.
    send.
    {deallocate,1}.
    return.
  {label,23}.
    {line,[{location,"grep.erl",15}]}.
    {badmatch,{x,0}}.
