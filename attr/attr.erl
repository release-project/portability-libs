%% Erlang node attributes library
%% Kenneth MacKenzie (kwxm@inf.ed.ac.uk)
%% Written at the University of Glasgow as part of the RELEASE project: www.release-project.eu.

-module(attr).
-export([attr_loop/1, %% We need to export this so we can spawn it
	 start/0, start/1, start/2, stop/0,
	 insert_attr/2, update_attr/2, delete_attr/1,
	 insert/2, update/2, delete/1,
	 request_attrs/2, request_attrs/3,
	 choose_nodes/2, choose_nodes/3
    ]).

-define (DEFAULT_TIMEOUT, 5000).
%% Give up waiting for an attribute if it hasn't arrived within 5 seconds.

-type node       () :: atom().
-type attr_name  () :: atom().

% External syntax for attribute definitions
-type extern_auto_attr () :: {automatic, {atom(), atom()}} | {automatic, {atom(), atom(), list(term())}}.  % {M,F} or {M,F,A}
-type extern_dyn_attr  () :: {dynamic, {atom(), atom()}} | {dynamic, {atom(), atom(), list(term())}}.
-type extern_attr_spec () :: term() | extern_dyn_attr() | extern_auto_attr().
-type extern_attr_def  () :: {attr_name(), extern_attr_spec()}.

% Internal format for attribute definitions
-type dyn_attr   () :: {dynamic, fun()} | {dynamic, fun(), list(term())}.
-type attr_spec  () :: term() | dyn_attr().
-type attr_def   () :: {attr_name(), attr_spec()}.  % Attribute definition
-type attr_val   () :: term().
-type attr       () :: {attr_name(), attr_val()}.   % Attribute after evaluation; value could be 'undefined'

%% What should we call {attr_name(), attr_val()}?


% ---------------- Built-in attributes ---------------- %

-spec builtins() -> [attr_def()].
builtins() -> [
	       {vm_num_processors, erlang:system_info(logical_processors_available)},
	       {mem_total,   dynattr:mem_total()},
	       {os_type,     os:type()},
	       {os_version,  os:version()},
	       {otp_release, erlang:system_info(otp_release)},  % etc...
	       {cpu_model_name, dynattr:cpu_model_name()},
	       {cpu_speed,   {dynamic, fun dynattr:cpu_speed/0 }},  % Static or dynamic?
	       {loadavg1,    {dynamic, fun dynattr:loadavg1/0  }}, 
	       {loadavg5,    {dynamic, fun dynattr:loadavg5/0  }}, 
	       {loadavg15,   {dynamic, fun dynattr:loadavg15/0 }}, 
	       {mem_free,    {dynamic, fun dynattr:mem_free/0  }},
	       {kernel_entities, {dynamic, fun dynattr:kernel_entities/0}},
	       {num_erlang_processes, {dynamic, fun dynattr:num_erlang_processes/0}},
	       {num_proc2, {dynamic, fun erlang:system_info/1, [process_count]}}
	       ].

%% We could also convert these into extern_attr_defs and move them to a separate configuration file.


% ---------------- Attribute server ---------------- %

-spec dump(ets:tid()) -> ok.
dump(Tid) ->  %% For debugging
    L = ets:tab2list(Tid),
    io:format ("Attributes on ~p~n", [node()]),
    io:format ("----~n"),
    lists:foreach (fun ({Name, Value}) ->
				io:format ("~p -> ~p~n", [Name,Value]) end, L),
    io:format ("----~n~n").
	
-spec lookup_attr(ets:tid(), attr_name()) -> attr().
lookup_attr (Tid, AttrName) ->
    case ets:lookup(Tid, AttrName) of
	[] -> {AttrName, undefined};
	[{AttrName, Val}] -> 
	    case Val of 
 		{dynamic, F} -> 
		    {AttrName, erlang:apply (F, [])};
		{dynamic, F, A} ->
		    {AttrName, erlang:apply (F, A)};
		_ -> {AttrName, Val}
	    end;
	X -> error ({too_many_entries, AttrName, X})
	     %% ^ Shouldn't happen, since the attribute table is supposed to be a set.
    end.
	    


% Server process which handles requests from other nodes for attribute values
-spec attr_loop(ets:tid()) -> ok.
attr_loop(Tid) ->
    receive  
	{Pid, Msg} when is_pid (Pid) ->
	    case Msg of
		{report, Key, AttrName} when is_atom (AttrName) ->
		    AttrVal = lookup_attr(Tid, AttrName),
		    Pid ! {attr_vals, Key, node(), [AttrVal]},
		    attr_loop(Tid);
		
		{report, Key, AttrNames} when is_list (AttrNames) ->
		    AttrVals = lists:map (fun(A) -> lookup_attr(Tid, A) end, AttrNames),
		    Pid ! {attr_vals, Key, node(), AttrVals},
		    attr_loop(Tid);
		%% ping -> Pid ! got_that, attr_loop(Tid);
		Msg -> 
		    Pid ! {unknown_action, node(), Msg},  %% or error?
		    attr_loop(Tid)
	    end;
	stop -> 
	    ets:delete(Tid), 
	    ok;
	dump -> 
	    dump(Tid), % for debugging
	    attr_loop(Tid);
	Msg1 -> 
	    io:format ("Unknown request: ~p~n", [Msg1]),
	    attr_loop(Tid)
    end.

% Do we want to do the responses in separate processes to speed up concurrent requests?
		
%% Convert external attrbute reperesentation to internal one.
-spec resolve_attr(extern_attr_def()) -> attr_def().
resolve_attr({AttrName, AttrSpec}) ->
    AttrVal = case AttrSpec of 	
	      %% We're not supposed to use erlang:apply ({M,F}) any more, but
	      %% file:consult rejects "fun erlang:now/0", for example (expr, not term)
		  {automatic, {M,F}} -> 
		      erlang:apply(fun M:F/0, []);   % Not erlang:apply (M,F,[]),
		  {automatic, {M,F,A}} ->
		      N = length(A),
		      erlang:apply(fun M:F/N, A);  
		  {dynamic, {M,F}} -> 
		      {dynamic, fun M:F/0};
		  {dynamic, {M,F,A}} ->
		      N = length(A),
		      {dynamic, fun M:F/N, A};
		  _ -> AttrSpec
	      end,
    {AttrName, AttrVal}.

% We should maybe report errors in case of malformed attributes


-spec register_attrs (ets:tid(), [extern_attr_def()]) -> true.
register_attrs(T,L) ->
    ets:insert(T,lists:map (fun resolve_attr/1, L)).

-spec register_attr (ets:tid(), extern_attr_def()) -> true.
register_attr(T,X) ->
    ets:insert(T,[resolve_attr(X)]).



%% User functions for inserting/updating/deleting attributes
%% programatically.  Note that if these are used wrongly (eg, if we
%% try to insert an attribute twice) then the server will crash.
%% It moight be simpler just to have a single function
%% which doesn't care whether the attribute exists or not.

-spec insert_attr(attr_name(), extern_attr_def()) -> true.
insert_attr(AttrName,AttrVal) -> 
    Tid = get(attr_tid),
    case ets:member(Tid, AttrName) of
	true ->    %% Fail if the attribute is already defined
	    error ({attr_exists, AttrName});
	false ->
	    register_attr(Tid, {AttrName, AttrVal})
    end.
    
-spec update_attr(attr_name(), extern_attr_def()) -> true.
update_attr(AttrName,AttrVal) -> 
    Tid = get(attr_tid),
    case ets:member(Tid, AttrName) of
	false ->    %% Fail if the attribute is NOT defined
	    error ({attr_not_set, AttrName}); 
	true ->
	    register_attr(Tid, {AttrName, AttrVal})
    end.

-spec delete_attr(attr_name()) -> true.
delete_attr(AttrName) -> 
    Tid = get(attr_tid),
    ets:delete(Tid, AttrName).
    

%% Aliases for the above functions. Things like attr:insert_attr are a bit repetitive.
%% The original names are included becuase they're in the paper; if
%% the paper's accepted we could change the names there and get rid of
%% the versions above.

-spec insert(attr_name(), extern_attr_def()) -> true.
insert(AttrName,AttrVal) -> insert_attr(AttrName,AttrVal).

-spec update(attr_name(), extern_attr_def()) -> true.
update(AttrName,AttrVal) -> update_attr(AttrName,AttrVal).

-spec delete(attr_name()) -> true.
delete(AttrName) -> delete_attr(AttrName).


% ---------------- Starting and stopping the attribute server ---------------- %

-spec running() -> boolean().
running() ->
    lists:member (attr_server, registered()).

-spec start_server (boolean(), file:filename()|none) -> ok.
start_server(UseBuiltins, ConfigFile) ->
    case running() of
	true -> {error, already_running};
	false ->
	    Tid = case ets:new(attr_info,[set,named_table,public]) of
		      badarg -> error ("Can't create attr_info");
		      T -> T
		  end,
	    
	    true = case UseBuiltins of
		     true -> register_attrs (Tid, builtins());
		     false -> true
		 end,

	    true = case ConfigFile of 
		     none -> true;
		       _ ->
			 Attrs = case file:consult(ConfigFile) of
				     {error, Msg} -> error ({attr_config_error, Msg});
				     {ok, [L]} -> L
				 end,
			 register_attrs (Tid, Attrs)
		 end,
	    ServerPid = spawn (?MODULE, attr_loop, [Tid]),
	    true = erlang:register(attr_server, ServerPid),
	    _ = put(attr_tid, Tid),
	    ok
    end.

%% Use built-in attributes only
-spec start() -> ok.
start() -> 
    start_server (true, none).


-spec start(file:filename()|nobuiltins) -> ok.
start(ConfigFile) when is_list(ConfigFile)-> 
    %% Use built-in attributes and those provided in configuration file
    start_server(true, ConfigFile);
start(nobuiltins) ->
    %% Start with a completely empty table
    start_server(false, none).

    
%% Use built-in attributes and those provided in configuration file
-spec start(file:filename(),nobuiltins) -> ok.
start(ConfigFile, nobuiltins) ->
    start_server(false, ConfigFile).  
    
-spec stop() -> stop | tuple().
stop() ->
    case running() of
	true -> attr_server ! stop;
	false -> {error, not_running}
    end.


% ------------------------------ Predicates ---------------------------------- %

-type oper       () :: eq|ne|lt|le|ge|gt.
-type pconst     () :: true|false|yes|no.
-type predicate  () :: {attr_name(), oper(), term()} | {attr_name(), pconst()}.

-spec get_value(attr_name(), [attr()]) -> term() | undefined.
get_value (Attr, Env) ->
    case lists:keyfind (Attr, 1, Env) of
	false -> undefined;
	{Attr, undefined} -> undefined;
	{Attr, Value} -> Value;
	_ -> undefined
    end.
	    
-spec get_attrname(predicate()) -> attr_name().
get_attrname (Predicate) ->
    element (1, Predicate).


-spec eval_predicate(predicate(), [attr()]) -> boolean().
eval_predicate (Pred, Env) ->
    case Pred of 
	{} -> true;
	_ -> Attr = get_attrname(Pred),
	     case get_value (Attr, Env) of
		 undefined -> false;
		 Val -> 
		     case Pred of
			 {_Attr, eq, K} -> Val == K;  
			 {_Attr, ne, K} -> Val /= K;
			 {_Attr, lt, K} -> Val <  K;
			 {_Attr, le, K} -> Val =< K;
			 {_Attr, gt, K} -> Val >  K;
			 {_Attr, ge, K} -> Val >= K;
			 {_Attr, true}  -> Val =:= true;
			 {_Attr, false} -> Val =:= false;
			 {_Attr, yes}   -> Val =:= yes;
			 {_Attr, no}    -> Val =:= no
		      %% It might be better to check the syntax of the predicates before querying other nodes
		     end
	     end
    end.


-spec check_predicates([predicate()], [attr()]) -> boolean().
check_predicates ([], _Attrs) -> true;
check_predicates ([Pred|Rest], Attrs) ->
    case eval_predicate (Pred, Attrs) of
	true -> check_predicates (Rest, Attrs);
	false -> false
    end.


% Given a list of {node, attr} pairs and a list of predicates, return
% all nodes whose attributes satisfy (all of) the predicates.

-spec check_attrs([{node(),[attr()]}], [predicate()], [node()]) -> [node()].
check_attrs ([], _, Acc) -> lists:reverse(Acc);

check_attrs ([{Node,Attrs}|Rest], Predicates, Acc) ->
    case check_predicates (Predicates, Attrs) of
	true  -> check_attrs(Rest, Predicates, [Node|Acc]);
	false -> check_attrs(Rest, Predicates, Acc)
    end.
	    
-spec check_attrs([{node(),[attr()]}], [predicate()]) -> [node()].
check_attrs (AttrList, Predicates) ->
    check_attrs (AttrList, Predicates, []).



% ---------------- Requesting attributes and choosing nodes ---------------- %     

-spec gather_attrs(list(), reference(), pos_integer(),
		   [{node(), [attr()]}]) -> [{node(),[attr()]}].
gather_attrs ([],_,_,Acc) ->
    lists:reverse(Acc);
gather_attrs ([_|T],Key,Timeout,Acc) ->
    % First list is original list of nodes: we're just using it to count the responses here.
    receive 
	{attr_vals, Key, Node, L} -> 
	    gather_attrs(T,Key,Timeout,[{Node,L}|Acc]);
	{unknown_action, Node, Msg} -> error ({unknown_action, Node, Msg})
    after Timeout -> [timeout|Acc]
    end.

% We're going to be in trouble if any of the nodes fails to respond,
% either because the node is down or it gets stuck while looking up a
% (dynamic) attribute.  We could ameliorate the former by pinging the 
% target first, but we'll still have problems if a node is slow to respond.
% Having nodes broadcast their attributes would help with this, since then 
% we could just query the attributes we have.

-spec request_attrs([node()], [attr_name()], non_neg_integer()) -> list().
request_attrs (Nodes, AttrNames, Timeout) ->
    Key = make_ref(),
    lists:foreach (fun(Node) -> 
			   case net_adm:ping(Node) of  %% Is this really necessary?
			       pong -> {attr_server, Node} ! {self(), {report, Key, AttrNames}};
			       pang -> error ({node_not_found, Node}) 
			   end
		   end, Nodes),
    gather_attrs (Nodes, Key, Timeout, []).

-spec request_attrs([node()], [attr_name()]) -> list().
request_attrs (Nodes, AttrNames) -> request_attrs(Nodes, AttrNames, ?DEFAULT_TIMEOUT).

-spec choose_nodes([node()], [predicate()], non_neg_integer()) -> [node()].
choose_nodes (Nodes, Predicates, Timeout) when is_list (Predicates) ->
    AttrNames = lists:map (fun get_attrname/1, Predicates),
    Attrs = request_attrs (Nodes, AttrNames, Timeout),
    check_attrs (Attrs, Predicates);
    
choose_nodes (Nodes, Predicate, Timeout) when is_tuple(Predicate) ->
    choose_nodes (Nodes, [Predicate], Timeout).


-spec choose_nodes([node()], [predicate()]) -> [node()].
choose_nodes (Nodes, Predicates) when is_list (Predicates) ->
    choose_nodes(Nodes, Predicates, ?DEFAULT_TIMEOUT);

choose_nodes (Nodes, Predicate) when is_tuple(Predicate) ->
    choose_nodes (Nodes, [Predicate], ?DEFAULT_TIMEOUT).

% eg choose_nodes (Nodes, [{os, eq, "Linux"}, {cpu_speed, ge, 2000}]).


