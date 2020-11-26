{module, lists}.  %% version = 0

{exports, [{foreach,2},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 8}.


{function, foreach, 2, 2}.
  {label,1}.
    {line,[{location,"lists.erl",29}]}.
    {func_info,{atom,lists},{atom,foreach},2}.
  {label,2}.
    {test,is_nonempty_list,{f,3},[{x,1}]}.
    {allocate,2,2}.
    {move,{x,0},{y,1}}.
    {get_list,{x,1},{x,0},{y,0}}.
    {move,{y,1},{x,1}}.
    {line,[{location,"lists.erl",30}]}.
    {call_fun,1}.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,0}}.
    {call_last,2,{f,2},2}.
  {label,3}.
    {test,is_nil,{f,1},[{x,1}]}.
    {move,{atom,ok},{x,0}}.
    return.


{function, module_info, 0, 5}.
  {label,4}.
    {line,[]}.
    {func_info,{atom,lists},{atom,module_info},0}.
  {label,5}.
    {move,{atom,lists},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 7}.
  {label,6}.
    {line,[]}.
    {func_info,{atom,lists},{atom,module_info},1}.
  {label,7}.
    {move,{x,0},{x,1}}.
    {move,{atom,lists},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
