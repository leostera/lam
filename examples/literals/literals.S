{module, literals}.  %% version = 0

{exports, [{main,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 7}.


{function, main, 1, 2}.
  {label,1}.
    {line,[{location,"literals.erl",4}]}.
    {func_info,{atom,literals},{atom,main},1}.
  {label,2}.
    {move,{literal,[ok,"hello",<<"world">>,10,10100,12345678901,true,
                    [1,2,[3]],
                    #{hello => world},
                    ok,ok]},
          {x,1}}.
    {move,{literal,<<"~p">>},{x,0}}.
    {line,[{location,"literals.erl",16}]}.
    {call_ext_only,2,{extfunc,io,format,2}}.


{function, module_info, 0, 4}.
  {label,3}.
    {line,[]}.
    {func_info,{atom,literals},{atom,module_info},0}.
  {label,4}.
    {move,{atom,literals},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 6}.
  {label,5}.
    {line,[]}.
    {func_info,{atom,literals},{atom,module_info},1}.
  {label,6}.
    {move,{x,0},{x,1}}.
    {move,{atom,literals},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
