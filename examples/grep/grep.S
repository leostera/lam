{module, grep}.  %% version = 0

{exports, [{main,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 18}.


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
    {make_fun2,{f,16},0,0,2}.
    {'%',{var_info,{x,0},[{fun_type,{t_atom,[ok]}}]}}.
    {line,[{location,"grep.erl",14}]}.
    {call_ext_only,1,{extfunc,erlang,spawn,1}}.


{function, main, 1, 8}.
  {label,7}.
    {line,[{location,"grep.erl",20}]}.
    {func_info,{atom,grep},{atom,main},1}.
  {label,8}.
    {test,is_nonempty_list,{f,7},[{x,0}]}.
    {allocate,1,1}.
    {get_list,{x,0},{x,0},{y,0}}.
    {make_fun2,{f,14},0,0,1}.
    {move,{y,0},{x,1}}.
    {line,[{location,"grep.erl",22}]}.
    {call_ext_last,2,{extfunc,lists,foreach,2},1}.


{function, module_info, 0, 10}.
  {label,9}.
    {line,[]}.
    {func_info,{atom,grep},{atom,module_info},0}.
  {label,10}.
    {move,{atom,grep},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 12}.
  {label,11}.
    {line,[]}.
    {func_info,{atom,grep},{atom,module_info},1}.
  {label,12}.
    {move,{x,0},{x,1}}.
    {move,{atom,grep},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-main/1-fun-0-', 2, 14}.
  {label,13}.
    {line,[{location,"grep.erl",21}]}.
    {func_info,{atom,grep},{atom,'-main/1-fun-0-'},2}.
  {label,14}.
    {call_only,2,{f,6}}.


{function, '-count/2-fun-0-', 2, 16}.
  {label,15}.
    {line,[{location,"grep.erl",14}]}.
    {func_info,{atom,grep},{atom,'-count/2-fun-0-'},2}.
  {label,16}.
    {allocate,2,2}.
    {move,{x,1},{y,0}}.
    {move,{x,0},{y,1}}.
    {line,[{location,"grep.erl",15}]}.
    {call_ext,1,{extfunc,file,read_file,1}}.
    {test,is_tagged_tuple,{f,17},[{x,0},2,{atom,ok}]}.
    {get_tuple_element,{x,0},1,{x,0}}.
    {move,{literal,[global]},{x,2}}.
    {move,{literal,<<" ">>},{x,1}}.
    {line,[{location,"grep.erl",16}]}.
    {call_ext,3,{extfunc,binary,split,3}}.
    {move,{x,0},{x,1}}.
    {move,{y,1},{x,2}}.
    {move,{y,0},{x,0}}.
    {call_last,3,{f,2},2}.
  {label,17}.
    {line,[{location,"grep.erl",15}]}.
    {badmatch,{x,0}}.
