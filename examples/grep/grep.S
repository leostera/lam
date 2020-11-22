{module, grep}.  %% version = 0

{exports, [{main,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 16}.


{function, print_matches, 3, 2}.
  {label,1}.
    {line,[{location,"grep.erl",6}]}.
    {func_info,{atom,grep},{atom,print_matches},3}.
  {label,2}.
    {test,is_nonempty_list,{f,4},[{x,1}]}.
    {get_list,{x,1},{x,3},{x,1}}.
    {test,is_eq_exact,{f,3},[{x,3},{x,0}]}.
    {allocate_heap,3,4,3}.
    {move,{x,1},{y,0}}.
    {move,{x,2},{y,1}}.
    {move,{x,0},{y,2}}.
    {put_list,{x,0},nil,{x,0}}.
    {put_list,{x,2},{x,0},{x,1}}.
    {move,{literal,"~p: ~p"},{x,0}}.
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


{function, count, 2, 6}.
  {label,5}.
    {line,[{location,"grep.erl",13}]}.
    {func_info,{atom,grep},{atom,count},2}.
  {label,6}.
    {allocate,2,2}.
    {move,{x,1},{y,0}}.
    {move,{x,0},{y,1}}.
    {line,[{location,"grep.erl",14}]}.
    {call_ext,1,{extfunc,file,read_file,1}}.
    {test,is_tagged_tuple,{f,7},[{x,0},2,{atom,ok}]}.
    {get_tuple_element,{x,0},1,{x,0}}.
    {move,{literal,[global]},{x,2}}.
    {move,{literal,<<" ">>},{x,1}}.
    {line,[{location,"grep.erl",15}]}.
    {call_ext,3,{extfunc,binary,split,3}}.
    {move,{x,0},{x,1}}.
    {move,{y,1},{x,2}}.
    {move,{y,0},{x,0}}.
    {call_last,3,{f,2},2}.
  {label,7}.
    {line,[{location,"grep.erl",14}]}.
    {badmatch,{x,0}}.


{function, main, 1, 9}.
  {label,8}.
    {line,[{location,"grep.erl",18}]}.
    {func_info,{atom,grep},{atom,main},1}.
  {label,9}.
    {test,is_nonempty_list,{f,8},[{x,0}]}.
    {allocate,1,1}.
    {get_list,{x,0},{x,0},{y,0}}.
    {make_fun2,{f,15},0,0,1}.
    {'%',{var_info,{x,0},[{fun_type,{t_atom,[ok]}}]}}.
    {move,{y,0},{x,1}}.
    {line,[{location,"grep.erl",20}]}.
    {call_ext_last,2,{extfunc,lists,foreach,2},1}.


{function, module_info, 0, 11}.
  {label,10}.
    {line,[]}.
    {func_info,{atom,grep},{atom,module_info},0}.
  {label,11}.
    {move,{atom,grep},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 13}.
  {label,12}.
    {line,[]}.
    {func_info,{atom,grep},{atom,module_info},1}.
  {label,13}.
    {move,{x,0},{x,1}}.
    {move,{atom,grep},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-main/1-fun-0-', 2, 15}.
  {label,14}.
    {line,[{location,"grep.erl",19}]}.
    {func_info,{atom,grep},{atom,'-main/1-fun-0-'},2}.
  {label,15}.
    {call_only,2,{f,6}}.
