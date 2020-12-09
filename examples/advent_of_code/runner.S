{module, runner}.  %% version = 0

{exports, [{main,1},{module_info,0},{module_info,1},{run_days,1},{run_one,1}]}.

{attributes, []}.

{labels, 15}.


{function, run_one, 1, 2}.
  {label,1}.
    {line,[{location,"runner.erl",9}]}.
    {func_info,{atom,runner},{atom,run_one},1}.
  {label,2}.
    {test,is_tuple,{f,1},[{x,0}]}.
    {test,test_arity,{f,1},[{x,0},2]}.
    {allocate_heap,1,2,1}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {get_tuple_element,{x,0},1,{y,0}}.
    {put_list,{x,1},nil,{x,1}}.
    {move,{literal,<<"Running day #~p...">>},{x,0}}.
    {line,[{location,"runner.erl",10}]}.
    {call_ext,2,{extfunc,io,format,2}}.
    {move,{y,0},{x,0}}.
    {trim,1,0}.
    {line,[{location,"runner.erl",11}]}.
    {call_fun,0}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{literal,<<"~p\n">>},{x,0}}.
    {line,[{location,"runner.erl",11}]}.
    {call_ext_last,2,{extfunc,io,format,2},0}.


{function, run_days, 1, 4}.
  {label,3}.
    {line,[{location,"runner.erl",14}]}.
    {func_info,{atom,runner},{atom,run_days},1}.
  {label,4}.
    {allocate,1,1}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,14},0,0,0}.
    {move,{y,0},{x,1}}.
    {line,[{location,"runner.erl",14}]}.
    {call_ext_last,2,{extfunc,lists,foreach,2},1}.


{function, main, 1, 6}.
  {label,5}.
    {line,[{location,"runner.erl",19}]}.
    {func_info,{atom,runner},{atom,main},1}.
  {label,6}.
    {allocate,0,0}.
    {move,nil,{x,1}}.
    {move,{literal,<<"==# Advent Of Code 2020! #==\n\n">>},{x,0}}.
    {line,[{location,"runner.erl",20}]}.
    {call_ext,2,{extfunc,io,format,2}}.
    {make_fun2,{f,12},0,0,0}.
    {test_heap,5,1}.
    {put_tuple2,{x,0},{list,[{integer,1},{x,0}]}}.
    {put_list,{x,0},nil,{x,0}}.
    {line,[{location,"runner.erl",24}]}.
    {call,1,{f,4}}.
    {move,nil,{x,1}}.
    {move,{literal,<<"\n\n">>},{x,0}}.
    {line,[{location,"runner.erl",25}]}.
    {call_ext_last,2,{extfunc,io,format,2},0}.


{function, module_info, 0, 8}.
  {label,7}.
    {line,[]}.
    {func_info,{atom,runner},{atom,module_info},0}.
  {label,8}.
    {move,{atom,runner},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 10}.
  {label,9}.
    {line,[]}.
    {func_info,{atom,runner},{atom,module_info},1}.
  {label,10}.
    {move,{x,0},{x,1}}.
    {move,{atom,runner},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-main/1-fun-0-', 0, 12}.
  {label,11}.
    {line,[{location,"runner.erl",21}]}.
    {func_info,{atom,runner},{atom,'-main/1-fun-0-'},0}.
  {label,12}.
    {line,[{location,"runner.erl",22}]}.
    {call_ext_only,0,{extfunc,day_1,run,0}}.


{function, '-run_days/1-fun-0-', 1, 14}.
  {label,13}.
    {line,[{location,"runner.erl",14}]}.
    {func_info,{atom,runner},{atom,'-run_days/1-fun-0-'},1}.
  {label,14}.
    {call_only,1,{f,2}}.
