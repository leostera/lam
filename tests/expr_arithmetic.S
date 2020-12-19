{module, expr_arithmetic}.  %% version = 0

{exports, [{main,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 9}.


{function, run, 5, 2}.
  {label,1}.
    {line,[{location,"expr_arithmetic.erl",5}]}.
    {func_info,{atom,expr_arithmetic},{atom,run},5}.
  {label,2}.
    {'%',{var_info,{x,0},[{type,{t_integer,{2112,2112}}}]}}.
    {'%',{var_info,{x,1},[{type,{t_integer,{7,7}}}]}}.
    {'%',{var_info,{x,2},[{type,{t_atom,[true]}}]}}.
    {'%',{var_info,{x,3},[{type,{t_atom,[false]}}]}}.
    {'%',{var_info,{x,4},[{type,{t_cons,{t_atom,[a_list]},nil}}]}}.
    {allocate_heap,1,4,5}.
    {put_list,{atom,true},{x,4},{x,0}}.
    {put_list,{integer,2112},{x,0},{y,0}}.
    {move,{x,4},{x,1}}.
    {move,{literal,[true]},{x,0}}.
    {line,[{location,"expr_arithmetic.erl",33}]}.
    {call_ext,2,{extfunc,erlang,'--',2}}.
    {move,{x,0},{x,1}}.
    {move,{literal,[2112]},{x,0}}.
    {line,[{location,"expr_arithmetic.erl",33}]}.
    {call_ext,2,{extfunc,erlang,'--',2}}.
    {test_heap,12,1}.
    {put_tuple2,{x,0},
                {list,[{literal,<<"Right associativity">>},{y,0},{x,0}]}}.
    {put_list,{x,0},
              {literal,[{<<"Operator precedence">>,-1,-1,-1},
                        {<<"Binary operations">>,2105,2119,14784,
                         [2112,7],
                         [2112],
                         301.7142857142857,true,false,true,false,false,false,
                         true,true,false,0,2119,270336,16,2119,301,true,5,
                         true}]},
              {x,0}}.
    {put_list,{literal,{<<"Left associativity">>,4224,1056.0,false,true,false,
                        0,1056,0,2115,2109,2115,16896,264,2115}},
              {x,0},
              {x,0}}.
    {put_list,{literal,{<<"Unary ops">>,2112,-2112,-2113,false}},{x,0},{x,0}}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{literal,"~p\n"},{x,0}}.
    {line,[{location,"expr_arithmetic.erl",6}]}.
    {call_ext_last,2,{extfunc,io,format,2},1}.


{function, main, 1, 4}.
  {label,3}.
    {line,[{location,"expr_arithmetic.erl",70}]}.
    {func_info,{atom,expr_arithmetic},{atom,main},1}.
  {label,4}.
    {move,{atom,true},{x,2}}.
    {move,{atom,false},{x,3}}.
    {move,{integer,7},{x,1}}.
    {move,{literal,[a_list]},{x,4}}.
    {move,{integer,2112},{x,0}}.
    {call_only,5,{f,2}}.


{function, module_info, 0, 6}.
  {label,5}.
    {line,[]}.
    {func_info,{atom,expr_arithmetic},{atom,module_info},0}.
  {label,6}.
    {move,{atom,expr_arithmetic},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 8}.
  {label,7}.
    {line,[]}.
    {func_info,{atom,expr_arithmetic},{atom,module_info},1}.
  {label,8}.
    {move,{x,0},{x,1}}.
    {move,{atom,expr_arithmetic},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
