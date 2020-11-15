{module, inf}.  %% version = 0

{exports, [{f,0},{main,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 9}.


{function, f, 0, 2}.
  {label,1}.
    {line,[{location,"inf.erl",8}]}.
    {func_info,{atom,inf},{atom,f},0}.
  {label,2}.
    {call_only,0,{f,2}}.


{function, main, 1, 4}.
  {label,3}.
    {line,[{location,"inf.erl",11}]}.
    {func_info,{atom,inf},{atom,main},1}.
  {label,4}.
    {call_only,0,{f,2}}.


{function, module_info, 0, 6}.
  {label,5}.
    {line,[]}.
    {func_info,{atom,inf},{atom,module_info},0}.
  {label,6}.
    {move,{atom,inf},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 8}.
  {label,7}.
    {line,[]}.
    {func_info,{atom,inf},{atom,module_info},1}.
  {label,8}.
    {move,{x,0},{x,1}}.
    {move,{atom,inf},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
