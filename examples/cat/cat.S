{module, cat}.  %% version = 0

{exports, [{main,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 8}.


{function, main, 1, 2}.
  {label,1}.
    {line,[{location,"cat.erl",5}]}.
    {func_info,{atom,cat},{atom,main},1}.
  {label,2}.
    {test,is_nonempty_list,{f,3},[{x,0}]}.
    {allocate,1,1}.
    {get_list,{x,0},{x,0},{y,0}}.
    {line,[{location,"cat.erl",7}]}.
    {call_ext,1,{extfunc,file,read_file,1}}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{literal,"~p\n"},{x,0}}.
    {line,[{location,"cat.erl",7}]}.
    {call_ext,2,{extfunc,io,format,2}}.
    {move,{y,0},{x,0}}.
    {call_last,1,{f,2},1}.
  {label,3}.
    {test,is_nil,{f,1},[{x,0}]}.
    {move,{atom,ok},{x,0}}.
    return.


{function, module_info, 0, 5}.
  {label,4}.
    {line,[]}.
    {func_info,{atom,cat},{atom,module_info},0}.
  {label,5}.
    {move,{atom,cat},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 7}.
  {label,6}.
    {line,[]}.
    {func_info,{atom,cat},{atom,module_info},1}.
  {label,7}.
    {move,{x,0},{x,1}}.
    {move,{atom,cat},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
