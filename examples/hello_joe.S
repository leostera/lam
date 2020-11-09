{module, hello_joe}.  %% version = 0

{exports, [{main,0},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 7}.


{function, main, 0, 2}.
  {label,1}.
    {line,[{location,"hello_joe.erl",7}]}.
    {func_info,{atom,hello_joe},{atom,main},0}.
  {label,2}.
    {move,nil,{x,1}}.
    {move,{literal,<<"Hello, Joe!">>},{x,0}}.
    {line,[{location,"hello_joe.erl",7}]}.
    {call_ext_only,2,{extfunc,io,format,2}}.


{function, module_info, 0, 4}.
  {label,3}.
    {line,[]}.
    {func_info,{atom,hello_joe},{atom,module_info},0}.
  {label,4}.
    {move,{atom,hello_joe},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 6}.
  {label,5}.
    {line,[]}.
    {func_info,{atom,hello_joe},{atom,module_info},1}.
  {label,6}.
    {move,{x,0},{x,1}}.
    {move,{atom,hello_joe},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
