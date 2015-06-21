%% Experimental code for representation of networks and calculations
%% involving metric properties.  Definitely not high-quality code.
%%
%% Kenneth MacKenzie (kwxm@inf.ed.ac.uk)
%% Written at the University of Glasgow as part of the RELEASE 
%% project: www.release-project.eu.

-module dist.
-export([init/1, nodenames/0, render/0, render/1,
	 distance/2, max_dist/0, min_dist/0,
	 choose_nodes/3, choose_nodes/2, 
	 choose_node/3, choose_node/2]).

-type vertex_name () :: atom().
-type network     () :: {'network', [network()]} 
		      | {'network', vertex_name(), [network()]} 
		      | {'node', node()}.

-type named_network() ::  {'network', vertex_name(), [network()]} 
			| {'node', node()}.  

%% We need names for some of the internal functions, like finding longest commom subpaths
%% Inputs may contain nameless nodes, but names will be generated for them by the code below.

%% ---------------- Functions for naming internal nodes ---------------- %%

-spec makename(integer()) -> atom().
makename(K) ->  %% Make a name like 'v42'
    list_to_atom (lists:flatten(io_lib:format ("<v~w>", [K]))).

-spec fix_network_list ([network()], integer(), [named_network()]) -> {[named_network()], integer()}.
fix_network_list (L,Counter,Acc) ->
    case L of 
	[] ->
	    {lists:reverse (Acc), Counter};
	[H|T] ->
	    {H1, Counter2} = fixnames (H,Counter),
	    fix_network_list (T, Counter2, [H1|Acc])
    end.
		
-spec fix_network_list ([network()], integer()) -> {[named_network()], integer()}.
fix_network_list (L,Counter) ->
     fix_network_list (L,Counter,[]).

-spec fixnames (network(), integer()) -> {named_network(), integer()}.
fixnames (N,Counter) -> % We could maybe use lists:foldl
    case N of
	{network, L} -> 
	    ThisName = makename (Counter),
	    {L2, Counter2} = fix_network_list (L, Counter+1),
	    {{network, ThisName, L2}, Counter2};
	{network, Name, L } -> 
	    {L2, Counter2} = fix_network_list (L, Counter),
	    {{network, Name, L2}, Counter2};
	{node, Name} -> {{node, Name}, Counter}
			
    end.
    
-spec fixnames (network()) -> named_network().
fixnames (N) -> 
    {N1, _Counter} = fixnames(N,1),
    N1.


%% ---------------- Inputting a network description ---------------- %%

-spec read_network (Filename) -> Network when
      Filename :: file:name(),
      Network  :: named_network().

read_network (F) ->
    {ok, L} = file:consult (F),
    case L of 
	[H] -> fixnames(H);
	_ -> {error, unconnected_graph}
    end.

-spec invert (network()) -> dict().
% Convert a newtwork into a map from node names to the path from the node to the root

invert(N) ->
    D = invert (N, dict:new(), []),
    dict:map (fun (_K,V) -> lists:reverse(V) end, D).

-spec invert (named_network(),dict(),list(atom())) -> dict().
invert (N,D,Path) ->
    case N of 
	{node, Name} -> dict:store (Name, [Name|Path], D);
	{network, _L} -> {error, no_label};
	{network, Name, L} -> lists:foldl (fun (NN,DD) -> invert (NN,DD,[Name|Path]) end, D, L)
end.

-spec init (file:name()) -> ok.
init(Filename) ->
    Network = read_network(Filename),
    put(network, Network),
    Dict = invert (Network),
    put(pathdict, Dict),
    Nodenames = dict:fetch_keys(Dict),  % all node names in network
    put (nodenames, Nodenames),
    ok.

-spec getq(atom()) -> any().
getq(A) ->
    case get(A) of
	undefined -> error (dist_library_not_initialised);
	V -> V
    end.

-spec pathdict () -> dict().
pathdict() -> getq(pathdict).
	
-spec nodenames () -> [node()].
nodenames() -> getq(nodenames).
	
-spec network () -> named_network().
network() -> getq(network).
	


%% ---------------- Outputting dot source ---------------- %%

-spec nameof (named_network()) -> atom().
nameof (X) ->
     case X of
	{network, Name, _L } -> Name;
	{node, Name} -> Name
     end. 


-spec ntd(N) -> Dot when
      N :: named_network(),
      Dot :: iolist().

ntd(N) ->
    case N of
	{network, Name, L} -> 
	    [lists:map (fun (X) -> io_lib:format ("  \"~s\" -> \"~s\";~n", [Name, nameof(X)]) end, L),
	    lists:map (fun ntd/1, L)];
	{node, Name} -> io_lib:format("  \"~s\" [shape=rectangle];~n", [Name])
			%% Leaves (ie, Erlang nodes) are rectangular, internal nodes are elliptical

    end.


-spec network_to_dot (N) -> Dot when
      N :: named_network(),
      Dot :: iolist(). 

network_to_dot (N) -> 
    ["digraph G {\n  rankdir=LR;\n", ntd(fixnames(N)),  "}\n"].
    % rankdir=LR makes the tree go sideways:  the test data 
    % was too wide to fit on the page in vertical orientation


-spec write_dot (named_network()) -> {ok, file:name()}| {error, atom()}.
write_dot(N) ->
     write_dot ("network.dot", N).

-spec write_dot (file:name(), named_network()) -> {ok, file:name()} | {error, atom()}.
write_dot (F,N) ->
    ok = file:write_file(F, network_to_dot(N)),
    {ok, F}.

-spec render() -> {ok, file:name()}.
render() ->
    write_dot(network()).

-spec render(file:name()) -> {ok, file:name()}.
render(F) ->
    write_dot(F,network()).




%% ---------------- Distance calculations ---------------- %%

% longest common prefix
-spec lcp (list(), list()) -> integer().

lcp(L1,L2) -> lcp (L1,L2,0).
lcp(L1,L2,N) ->
    case L1 of 
	[] -> N;
	[H1|T1] ->
	    case L2 of 
		[] -> N;		     
		[H2|T2] -> 
		    if H1 =:= H2 -> lcp (T1, T2, N+1);
		       true -> N
		    end
	    end
    end.
				    

-spec path_to_node(node(), dict()) -> list()|{error, {node_not_found, node()}}.    
path_to_node(Node, D) ->	
    case dict:find (Node,D) of
	error ->
	    {error, {node_not_found, Node}};
	{ok, P} -> P
    end.
		

-spec distance(node(), node(), dict()) -> float().
distance (X,Y,D) -> 
    PX = path_to_node(X,D),
    PY = path_to_node(Y,D),
    case PX of 
	{error, _} -> PX;
	_ -> 
	    case PY of {error, _} -> PY;
		_ -> % We have to go through this rigmarole to detect the case of identical missing nodes
		    if X =:= Y -> 0;
		       true -> 
			    N = lcp (PX, PY),
			    math:pow(2,1-N) 
			    % 1-N because lcp gives you the number of nodes in the longest common prefix,
			    % which is one more than the number of edges.
		    end
	    end
    end.

-spec distance(node(), node()) -> float().
distance (X,Y) -> distance(X,Y,pathdict()).


%% ---------------- Open/closed balls, spheres ---------------- %%

%% Open ball in a specified set of nodes
-spec open_ball(node(), [node()], float(), dict()) -> [node()].
open_ball (X,L,R,D) ->
    [Y || Y <- L, distance (X,Y,D) < R].
    
% %% Open ball in entire set of nodes
% -spec open_ball(node(), float()) -> [node()].
% open_ball (X,R) ->
%    Nodes = nodenames(),
%    D = pathdict(),
%    open_ball(X, Nodes, R, D).
    
%% Closed ball in a specified set of nodes
-spec closed_ball(node(), [node()], float(), dict()) -> [node()].
closed_ball (X,L,R,D) ->
    [Y || Y <- L, distance (X,Y,D) =< R].

% %% Closed ball in entire set of nodes
% -spec closed_ball(node(), float()) -> [node()].
% closed_ball (X,R) ->
%     Nodes = nodenames(),
%     D = pathdict(),
%     open_ball(X, Nodes, R, D).
    

%% Sphere in a specified set of nodes
-spec sphere(node(), [node()], float(), dict()) -> [node()].
sphere (X,L,R,D) ->
    [Y || Y <- L, distance (X,Y,D) == R].

% -spec sphere(node(), float()) -> [node()].
% sphere (X,R) ->
%     Nodes = nodenames(),
%     D = pathdict(),
%     sphere (X, Nodes, R, D).
    

%% Diameter of entire space
-spec diameter(dict()) -> float().    
diameter (D) -> % Inefficient, but never mind
    Nodes = dict:fetch_keys(D),
    lists:max ([distance(X,Y,D) || X <- Nodes,
				   Y <- Nodes]).

-spec granularity(dict()) -> float().
granularity (D) -> % Inefficient, but never mind
    Nodes = dict:fetch_keys(D),
    lists:min ([distance(X,Y,D) || X <- Nodes,
				   Y <- Nodes,
				   X =/= Y]).

-spec max_dist() -> float().
max_dist() ->
    diameter(pathdict()).


-spec min_dist() -> float().
min_dist() ->
    granularity(pathdict()).




%% ------------------------------ Predicates ---------------------------------- %%

-type oper       () :: eq|ne|lt|le|ge|gt.
-type predicate  () :: {dist, oper(), number()}.
-type predicates () :: predicate()|[predicate()].

%% ---------------- Choosing nodes ---------------- %%

%% Choose nodes from a list wrt a single distance predicate relative to a given node.
-spec filter_nodes(node(), [node()], predicate()) -> [node()].
filter_nodes (Node, L, Pred) ->
    D = pathdict(),
    case Pred of
	{dist, eq, R} -> sphere      (Node, L, R, D);  
	{dist, lt, R} -> open_ball   (Node, L, R, D);
	{dist, le, R} -> closed_ball (Node, L, R, D);
	{dist, ne, R} -> L -- sphere      (Node, L, R, D);  
	{dist, gt, R} -> L -- closed_ball (Node, L, R, D);  %% Nodes which don't satisfy {dist, le, R}
	{dist, ge, R} -> L -- open_ball   (Node, L, R, D)   %% Nodes which don't satisfy {dist, lt, R}
    end.

%% Choose nodes from a list wrt a conjunction of distance predicates relative to a given node.
%% Eg, you could use this to choose some nodes whose distances from the current node lie between a and b

-spec choose_nodes(node(), [node()], predicates()) -> [node()].
choose_nodes (Node, L, Preds) when is_list(Preds) ->
    lists:foldl (fun (Pred, Acc) -> filter_nodes(Node,Acc,Pred) end, L, Preds);

choose_nodes (Node, L, Pred) when is_tuple(Pred) ->
    filter_nodes(Node, L, Pred).

%% Choose a single (random) ndoe wrt a given predicate

-spec random_element(list()) -> any()|none.
random_element(L) ->
    case L of 
	[] -> none;
	_ ->
	    N = random:uniform(length(L)),
	    lists:nth(N,L)
    end.

-spec choose_node (node(), [node()], predicates()) -> node()|none.
choose_node(N, L, P) ->
     random_element(choose_nodes(N,L,P)).

%% Special cases with default set of nodes equal to all nodes

-spec choose_nodes(node(), predicates()) -> [node()].
choose_nodes(Node,P) -> choose_nodes(Node, nodenames(), P).
    
-spec choose_node(node(), predicates()) -> node()|none.
choose_node(Node,P) -> choose_node(Node, nodenames(), P).
    
