{module, grep}.  %% version = 0

{exports, [{main,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 13}.


{function, count, 3, 2}.
  {label,1}.
    {line,[{location,"grep.erl",6}]}.
    {func_info,{atom,grep},{atom,count},3}.
  {label,2}.
    {allocate,0,1}.
    {make_fun2,{f,12},0,0,1}.
    {line,[{location,"grep.erl",7}]}.
    {call_ext,1,{extfunc,erlang,spawn,1}}.
    {move,{atom,ok},{x,0}}.
    {deallocate,0}.
    return.


{function, main, 1, 4}.
  {label,3}.
    {line,[{location,"grep.erl",17}]}.
    {func_info,{atom,grep},{atom,main},1}.
  {label,4}.
    {test,is_nonempty_list,{f,3},[{x,0}]}.
    {allocate,1,1}.
    {get_list,{x,0},{x,0},{y,0}}.
    {bif,self,{f,0},[],{x,1}}.
    {make_fun2,{f,10},0,0,2}.
    {'%',{var_info,{x,0},[{fun_type,{t_atom,[ok]}}]}}.
    {move,{y,0},{x,1}}.
    {line,[{location,"grep.erl",20}]}.
    {call_ext_last,2,{extfunc,lists,foreach,2},1}.


{function, module_info, 0, 6}.
  {label,5}.
    {line,[]}.
    {func_info,{atom,grep},{atom,module_info},0}.
  {label,6}.
    {move,{atom,grep},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 8}.
  {label,7}.
    {line,[]}.
    {func_info,{atom,grep},{atom,module_info},1}.
  {label,8}.
    {move,{x,0},{x,1}}.
    {move,{atom,grep},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-main/1-fun-0-', 3, 10}.
  {label,9}.
    {line,[{location,"grep.erl",19}]}.
    {func_info,{atom,grep},{atom,'-main/1-fun-0-'},3}.
  {label,10}.
    {call_only,3,{f,2}}.


{function, '-count/3-fun-0-', 1, 12}.
  {label,11}.
    {line,[{location,"grep.erl",8}]}.
    {func_info,{atom,grep},{atom,'-count/3-fun-0-'},1}.
  {label,12}.
    {test_heap,4,1}.
    {bif,self,{f,0},[],{x,1}}.
    {put_list,{x,0},nil,{x,0}}.
    {put_list,{x,1},{x,0},{x,1}}.
    {move,{literal,<<"I'm: ~p, and will read: ~p">>},{x,0}}.
    {line,[{location,"grep.erl",9}]}.
    {call_ext_only,2,{extfunc,io,format,2}}.
