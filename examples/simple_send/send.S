{module, send}.  %% version = 0

{exports, [{main,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 9}.


{function, main, 1, 2}.
  {label,1}.
    {line,[{location,"send.erl",5}]}.
    {func_info,{atom,send},{atom,main},1}.
  {label,2}.
    {test,is_nonempty_list,{f,1},[{x,0}]}.
    {allocate,0,1}.
    {get_hd,{x,0},{x,1}}.
    {bif,self,{f,0},[],{x,0}}.
    {line,[{location,"send.erl",7}]}.
    send.
  {label,3}.
    {loop_rec,{f,4},{x,0}}.
    {test_heap,2,1}.
    remove_message.
    {put_list,{x,0},nil,{x,1}}.
    {move,{literal,<<"~p">>},{x,0}}.
    {line,[{location,"send.erl",9}]}.
    {call_ext_last,2,{extfunc,io,format,2},0}.
  {label,4}.
    {wait,{f,3}}.


{function, module_info, 0, 6}.
  {label,5}.
    {line,[]}.
    {func_info,{atom,send},{atom,module_info},0}.
  {label,6}.
    {move,{atom,send},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 8}.
  {label,7}.
    {line,[]}.
    {func_info,{atom,send},{atom,module_info},1}.
  {label,8}.
    {move,{x,0},{x,1}}.
    {move,{atom,send},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
