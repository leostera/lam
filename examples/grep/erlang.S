{module, erlang}.  %% version = 0

{exports, [{list_to_integer,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 7}.


{function, list_to_integer, 1, 2}.
  {label,1}.
    {line,[{location,"erlang.erl",27}]}.
    {func_info,{atom,erlang},{atom,list_to_integer},1}.
  {label,2}.
    {move,{atom,undefined},{x,0}}.
    {line,[{location,"erlang.erl",28}]}.
    {call_ext_only,1,{extfunc,erlang,nif_error,1}}.


{function, module_info, 0, 4}.
  {label,3}.
    {line,[]}.
    {func_info,{atom,erlang},{atom,module_info},0}.
  {label,4}.
    {move,{atom,erlang},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 6}.
  {label,5}.
    {line,[]}.
    {func_info,{atom,erlang},{atom,module_info},1}.
  {label,6}.
    {move,{x,0},{x,1}}.
    {move,{atom,erlang},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
