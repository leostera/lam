{module, empty}.  %% version = 0

{exports, [{main,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 7}.


{function, main, 1, 2}.
  {label,1}.
    {line,[{location,"empty.erl",3}]}.
    {func_info,{atom,empty},{atom,main},1}.
  {label,2}.
    {move,{atom,ok},{x,0}}.
    return.


{function, module_info, 0, 4}.
  {label,3}.
    {line,[]}.
    {func_info,{atom,empty},{atom,module_info},0}.
  {label,4}.
    {move,{atom,empty},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 6}.
  {label,5}.
    {line,[]}.
    {func_info,{atom,empty},{atom,module_info},1}.
  {label,6}.
    {move,{x,0},{x,1}}.
    {move,{atom,empty},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
