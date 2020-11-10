{module, holder}.  %% version = 0

{exports, [{do_work,0},
           {handle_message,2},
           {loop,2},
           {module_info,0},
           {module_info,1},
           {start,1}]}.

{attributes, []}.

{labels, 21}.


{function, handle_message, 2, 2}.
  {label,1}.
    {line,[{location,"holder.erl",19}]}.
    {func_info,{atom,holder},{atom,handle_message},2}.
  {label,2}.
    {test,is_tuple,{f,8},[{x,0}]}.
    {test,test_arity,{f,8},[{x,0},2]}.
    {test,is_tuple,{f,6},[{x,1}]}.
    {test,is_tagged_tuple,{f,7},[{x,1},2,{atom,some}]}.
    {get_tuple_element,{x,1},1,{x,2}}.
    {test,is_tuple,{f,5},[{x,2}]}.
    {test,test_arity,{f,7},[{x,2},2]}.
    {get_tuple_element,{x,2},0,{x,3}}.
    {get_tuple_element,{x,2},1,{x,2}}.
    {select_val,{x,3},{f,7},{list,[{atom,add},{f,4},{atom,hello},{f,3}]}}.
  {label,3}.
    {test_heap,3,3}.
    {get_tuple_element,{x,0},1,{x,0}}.
    {put_tuple2,{x,0},{list,[{x,2},{x,0}]}}.
    return.
  {label,4}.
    {test_heap,3,3}.
    {get_tuple_element,{x,0},0,{x,0}}.
    {put_tuple2,{x,0},{list,[{x,0},{x,2}]}}.
    return.
  {label,5}.
    {test,is_eq_exact,{f,7},[{x,2},{atom,reset}]}.
    {move,{literal,{<<>>,0}},{x,0}}.
    return.
  {label,6}.
    {test,is_eq_exact,{f,7},[{x,1},{atom,none}]}.
    return.
  {label,7}.
    {line,[{location,"holder.erl",21}]}.
    {case_end,{x,1}}.
  {label,8}.
    {line,[{location,"holder.erl",20}]}.
    {badmatch,{x,0}}.


{function, loop, 2, 10}.
  {label,9}.
    {line,[{location,"holder.erl",29}]}.
    {func_info,{atom,holder},{atom,loop},2}.
  {label,10}.
    {allocate_heap,2,2,2}.
    {move,{x,1},{y,0}}.
    {move,{x,0},{y,1}}.
    {put_list,{x,1},nil,{x,1}}.
    {move,{literal,<<"current_state: ~p\n">>},{x,0}}.
    {line,[{location,"holder.erl",30}]}.
    {call_ext,2,{extfunc,io,format,2}}.
    {move,{y,1},{x,1}}.
    {move,{literal,{bounded,5000}},{x,0}}.
    {line,[{location,"holder.erl",31}]}.
    {call_fun,1}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {trim,1,1}.
    {line,[{location,"holder.erl",32}]}.
    {call,2,{f,2}}.
    {'%',{var_info,{x,0},[{type,{t_tuple,2,true,#{}}}]}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {call_last,2,{f,10},1}.


{function, start, 1, 12}.
  {label,11}.
    {line,[{location,"holder.erl",36}]}.
    {func_info,{atom,holder},{atom,start},1}.
  {label,12}.
    {make_fun2,{f,20},0,0,1}.
    {'%',{var_info,{x,0},[{fun_type,none}]}}.
    {line,[{location,"holder.erl",36}]}.
    {call_ext_only,1,{extfunc,process,make,1}}.


{function, do_work, 0, 14}.
  {label,13}.
    {line,[{location,"holder.erl",41}]}.
    {func_info,{atom,holder},{atom,do_work},0}.
  {label,14}.
    {allocate,0,0}.
    {move,{literal,{<<"hi">>,0}},{x,0}}.
    {line,[{location,"holder.erl",42}]}.
    {call,1,{f,12}}.
    {move,{literal,{hello,<<"joe">>}},{x,1}}.
    {line,[{location,"holder.erl",43}]}.
    {call_ext_last,2,{extfunc,erlang,send,2},0}.


{function, module_info, 0, 16}.
  {label,15}.
    {line,[]}.
    {func_info,{atom,holder},{atom,module_info},0}.
  {label,16}.
    {move,{atom,holder},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 18}.
  {label,17}.
    {line,[]}.
    {func_info,{atom,holder},{atom,module_info},1}.
  {label,18}.
    {move,{x,0},{x,1}}.
    {move,{atom,holder},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-start/1-fun-0-', 3, 20}.
  {label,19}.
    {line,[{location,"holder.erl",36}]}.
    {func_info,{atom,holder},{atom,'-start/1-fun-0-'},3}.
  {label,20}.
    {move,{x,1},{x,0}}.
    {move,{x,2},{x,1}}.
    {call_only,2,{f,10}}.
