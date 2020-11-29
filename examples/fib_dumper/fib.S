{module, fib}.  %% version = 0

{exports, [{main,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 12}.


{function, add_entry, 2, 2}.
  {label,1}.
    {line,[{location,"fib.erl",5}]}.
    {func_info,{atom,fib},{atom,add_entry},2}.
  {label,2}.
    {allocate,2,2}.
    {move,{x,1},{y,0}}.
    {move,{x,0},{y,1}}.
    {move,{literal,<<"div">>},{x,0}}.
    {line,[{location,"fib.erl",6}]}.
    {call_ext,1,{extfunc,dom_document,create_element,1}}.
    {move,{y,0},{x,1}}.
    {move,{x,0},{y,0}}.
    {line,[{location,"fib.erl",7}]}.
    {call_ext,2,{extfunc,dom_element,set_inner_text,2}}.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,0}}.
    {line,[{location,"fib.erl",8}]}.
    {call_ext_last,2,{extfunc,dom_element,append_child,2},2}.


{function, fib, 4, 4}.
  {label,3}.
    {line,[{location,"fib.erl",10}]}.
    {func_info,{atom,fib},{atom,fib},4}.
  {label,4}.
    {'%',{var_info,{x,1},[{type,{t_integer,any}}]}}.
    {'%',{var_info,{x,2},[{type,{t_integer,any}}]}}.
    {'%',{var_info,{x,3},[{type,{t_integer,any}}]}}.
    {allocate,4,4}.
    {move,{x,3},{y,0}}.
    {move,{x,2},{y,1}}.
    {move,{x,1},{y,2}}.
    {move,{x,0},{y,3}}.
    {move,{x,3},{x,0}}.
    {line,[{location,"fib.erl",11}]}.
    {call_ext,1,{extfunc,erlang,integer_to_binary,1}}.
    {move,{x,0},{x,1}}.
    {move,{y,3},{x,0}}.
    {line,[{location,"fib.erl",11}]}.
    {call_ext,2,{extfunc,dom_element,set_inner_text,2}}.
    {test,is_ge,{f,5},[{integer,0},{y,2}]}.
    {move,{atom,ok},{x,0}}.
    {deallocate,4}.
    return.
  {label,5}.
    {line,[{location,"fib.erl",14}]}.
    {gc_bif,'-',{f,0},0,[{y,2},{integer,1}],{x,0}}.
    {line,[{location,"fib.erl",14}]}.
    {gc_bif,'+',{f,0},1,[{y,0},{y,1}],{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{y,1},{x,3}}.
    {move,{y,3},{x,0}}.
    {call_last,4,{f,4},4}.


{function, main, 1, 7}.
  {label,6}.
    {line,[{location,"fib.erl",17}]}.
    {func_info,{atom,fib},{atom,main},1}.
  {label,7}.
    {allocate_zero,3,0}.
    {move,nil,{x,1}}.
    {move,{literal,<<"Starting...">>},{x,0}}.
    {line,[{location,"fib.erl",18}]}.
    {call_ext,2,{extfunc,io,format,2}}.
    {move,{literal,<<"lam">>},{x,0}}.
    {line,[{location,"fib.erl",19}]}.
    {call_ext,1,{extfunc,dom_document,get_element_by_id,1}}.
    {move,{x,0},{y,2}}.
    {line,[{location,"fib.erl",21}]}.
    {call_ext,0,{extfunc,date,now,0}}.
    {move,{x,0},{y,1}}.
    {line,[{location,"fib.erl",22}]}.
    {call_ext,1,{extfunc,erlang,integer_to_binary,1}}.
    {move,{x,0},{x,1}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"fib.erl",22}]}.
    {call,2,{f,2}}.
    {move,{literal,<<"div">>},{x,0}}.
    {line,[{location,"fib.erl",25}]}.
    {call_ext,1,{extfunc,dom_document,create_element,1}}.
    {move,{x,0},{y,0}}.
    {move,{literal,<<"unstarted">>},{x,1}}.
    {line,[{location,"fib.erl",26}]}.
    {call_ext,2,{extfunc,dom_element,set_inner_text,2}}.
    {move,{y,0},{x,1}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"fib.erl",27}]}.
    {call_ext,2,{extfunc,dom_element,append_child,2}}.
    {move,{integer,1},{x,2}}.
    {move,{integer,10000},{x,1}}.
    {move,{integer,0},{x,3}}.
    {move,{y,0},{x,0}}.
    {trim,1,2}.
    {line,[{location,"fib.erl",28}]}.
    {call,4,{f,4}}.
    {'%',{var_info,{x,0},[{type,{t_atom,[ok]}}]}}.
    {line,[{location,"fib.erl",30}]}.
    {call_ext,0,{extfunc,date,now,0}}.
    {line,[{location,"fib.erl",30}]}.
    {gc_bif,'-',{f,0},1,[{x,0},{y,0}],{x,0}}.
    {trim,1,1}.
    {line,[{location,"fib.erl",31}]}.
    {call_ext,1,{extfunc,erlang,integer_to_binary,1}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {call_last,2,{f,2},1}.


{function, module_info, 0, 9}.
  {label,8}.
    {line,[]}.
    {func_info,{atom,fib},{atom,module_info},0}.
  {label,9}.
    {move,{atom,fib},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 11}.
  {label,10}.
    {line,[]}.
    {func_info,{atom,fib},{atom,module_info},1}.
  {label,11}.
    {move,{x,0},{x,1}}.
    {move,{atom,fib},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
