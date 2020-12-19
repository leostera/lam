{module, expr_arithmetic}.  %% version = 0

{exports, [{main,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 7}.


{function, main, 1, 2}.
  {label,1}.
    {line,[{location,"expr_arithmetic.erl",5}]}.
    {func_info,{atom,expr_arithmetic},{atom,main},1}.
  {label,2}.
    {move,{literal,[[{<<"Unary ops">>,2112,-2112,-2113,false},
                     {<<"Left associativity">>,4224,1056.0,false,true,false,0,
                      1056,0,2115,2109,2115,16896,264,2115},
                     {<<"Right associativity">>,[2112,true,a_list],[2112]},
                     {<<"Operator precedence">>,-1,-1,-1},
                     {<<"Binary operations">>,2105,2119,14784,
                      [2112,7],
                      [2112],
                      301.7142857142857,true,false,true,false,false,false,
                      true,true,false,0,2119,270336,16,2119,301,true,5,
                      true}]]},
          {x,1}}.
    {move,{literal,"~p\n"},{x,0}}.
    {line,[{location,"expr_arithmetic.erl",11}]}.
    {call_ext_only,2,{extfunc,io,format,2}}.


{function, module_info, 0, 4}.
  {label,3}.
    {line,[]}.
    {func_info,{atom,expr_arithmetic},{atom,module_info},0}.
  {label,4}.
    {move,{atom,expr_arithmetic},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 6}.
  {label,5}.
    {line,[]}.
    {func_info,{atom,expr_arithmetic},{atom,module_info},1}.
  {label,6}.
    {move,{x,0},{x,1}}.
    {move,{atom,expr_arithmetic},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
