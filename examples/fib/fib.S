{module, fib}.  %% version = 0

{exports, [{fib,3},{main,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 10}.


{function, fib, 3, 2}.
  {label,1}.
    {line,[{location,"fib.erl",8}]}.
    {func_info,{atom,fib},{atom,fib},3}.
  {label,2}.
    {test,is_ge,{f,3},[{integer,0},{x,0}]}.
    {test_heap,2,3}.
    {put_list,{x,2},nil,{x,1}}.
    {move,{literal,<<"~p">>},{x,0}}.
    {line,[{location,"fib.erl",10}]}.
    {call_ext_only,2,{extfunc,io,format,2}}.
  {label,3}.
    {line,[{location,"fib.erl",11}]}.
    {gc_bif,'-',{f,0},3,[{x,0},{integer,1}],{x,0}}.
    {line,[{location,"fib.erl",11}]}.
    {gc_bif,'+',{f,0},3,[{x,2},{x,1}],{x,2}}.
    {swap,{x,1},{x,2}}.
    {call_only,3,{f,2}}.


{function, main, 1, 5}.
  {label,4}.
    {line,[{location,"fib.erl",15}]}.
    {func_info,{atom,fib},{atom,main},1}.
  {label,5}.
    {test,is_nonempty_list,{f,4},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_nil,{f,4},[{x,2}]}.
    {allocate,0,2}.
    {move,{x,1},{x,0}}.
    {line,[{location,"fib.erl",15}]}.
    {call_ext,1,{extfunc,erlang,list_to_integer,1}}.
    {move,{integer,0},{x,2}}.
    {move,{integer,1},{x,1}}.
    {call_last,3,{f,2},0}.


{function, module_info, 0, 7}.
  {label,6}.
    {line,[]}.
    {func_info,{atom,fib},{atom,module_info},0}.
  {label,7}.
    {move,{atom,fib},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 9}.
  {label,8}.
    {line,[]}.
    {func_info,{atom,fib},{atom,module_info},1}.
  {label,9}.
    {move,{x,0},{x,1}}.
    {move,{atom,fib},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
