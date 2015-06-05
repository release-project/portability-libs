%% Built-in Erlang node attributes for Linux.
%% Kenneth MacKenzie (kwxm@inf.ed.ac.uk)
%% Written at the University of Glasgow as part of the RELEASE project: www.release-project.eu.

%% http://www.erlang.org/doc/efficiency_guide/functions.html says not 
%% to use {M, F}, but instead fun M:F/k.
%% ... except that file:consult fails when it gets such a term.
%% It looks as if fun M:F/k is in fact an expression rather than a term

-module(dynattr).
-export([loadavg1/0, loadavg5/0, loadavg15/0, kernel_entities/0,
	 num_erlang_processes/0, mem_total/0, mem_free/0, cpu_speed/0, cpu_model_name/0]).


-type scan_item () :: atom() | string() | number().  %% The things that io:fread can actually return.

-type string1() :: [1..255,...].                     
% Dialyzer -Wunderspecs complains if you use string() in some of the
% specs below (eg, find_one_entry), but that seems to be because the
% relevant functions are not exported and are only ever called with
% literal strings where it can see that the characters are from a
% restricted subset.

%% Scan through file looking for a line beginning with Prefix, then return rest of line.
-spec find_entry(string1(), file:io_device()) -> {ok, string()} | not_found.
find_entry(Prefix, Dev) ->
    case file:read_line(Dev) of
	eof -> not_found;
	{error,_} -> not_found;
	{ok, Data} -> 
	    case lists:prefix (Prefix, Data) of
		true -> 
		    {ok, string:substr(Data, 1+length(Prefix))};
		false -> 
		    find_entry(Prefix, Dev)
	    end
    end.


%% Scan through file looking for a line beginning with Prefix, 
%% then scan items from rest of line according to Pattern.

-spec find_entry(string1(), io:format(), file:io_device()) -> [scan_item()] | undefined.
find_entry(Prefix, Pattern, Dev) ->
    case find_entry(Prefix, Dev) of
	not_found -> 
	    undefined;
	{ok, Rest} -> 
	    case io_lib:fread(Pattern, Rest) of
		{ok, L, _} -> L;
		_ -> undefined
	    end
    end.
	    
%% Scan through file looking for a line beginning with Prefix, 
%% then scan a single item from rest of line according to Pattern.
%% Should only be used with patterns containing a single ~ thing.

-spec find_one_entry(string1(), io:format(), file:io_device()) -> scan_item() | undefined. 
%% 'undefined' is in fact redundant here, but I've left it in for documentation purposes.

find_one_entry(Prefix, Pattern, Dev) ->
    case find_entry(Prefix, Pattern, Dev) of
%%	[X] -> io:format ("* ~p~n", [X]),X;
	[X] -> X;
	_ -> undefined
    end.



%% Some sample dynamic attributes

% Load averages: man page for /proc says
%
%   The first three fields in this file are load average figures giving
%   the number of jobs in the run queue (state R) or waiting for disk I/O
%   (state D) averaged over 1, 5, and 15 minutes.  They are the same as
%   the load average numbers given by uptime(1) and other programs.  The
%   fourth field consists of two numbers separated by a slash (/).  The
%   first of these is the number of currently runnable kernel scheduling
%   entities (processes, threads).  The value after the slash is the
%   number of kernel scheduling entities that currently exist on the
%   system.  The fifth field is the PID of the process that was most
%   recently created on the system.


-spec get_loadinfo () -> list(number()) | undefined.
get_loadinfo () ->
    case file:open("/proc/loadavg", [read]) of
	{error, _} -> undefined;
	{ok, Dev} ->
	    Result =
		case file:read_line (Dev) of
		    {ok, Data} ->
			case io_lib:fread("~f~f~f~d/~d~d\n", Data) of
			    {ok, L, []} -> L;
			    _ -> undefined  
			end;
			% We require precisely the form given in the man page; this
			% may be different for other Linux/Unix versions
		    _ -> undefined
		end,
	    file:close(Dev),
	    Result
    end.

-spec loadavg1 () -> float() | undefined.
loadavg1 () ->   
    case get_loadinfo() of
	[L1,_,_,_,_,_] -> L1;
	_ -> undefined
    end.
			       
-spec loadavg5 () -> float() | undefined.
loadavg5 () ->   
    case get_loadinfo() of
	[_,L5,_,_,_,_] -> L5;
	_ -> undefined
    end.

-spec loadavg15 () -> float() | undefined.
loadavg15 () ->   
    case get_loadinfo() of
	[_,_, L15,_,_,_] -> L15;
	_ -> undefined
    end.
	    
-spec kernel_entities() -> {integer(),integer()} | undefined.
kernel_entities() ->
    case get_loadinfo() of
	[_, _, _, E1, E2,_] -> {E1, E2};
	_ -> undefined
    end.
    
-spec num_erlang_processes () -> integer().
num_erlang_processes() -> erlang:system_info(process_count).


-spec mem_total () -> integer() | undefined.
mem_total () ->
    case file:open("/proc/meminfo", [read]) of
	{error, _} -> {undefined, undefined};
	{ok, Dev} ->
	    Result = find_one_entry ("MemTotal:", "~d kB", Dev),
	    file:close(Dev),
	    Result
    end.

-spec mem_free () -> integer() | undefined.
mem_free () ->
    case file:open("/proc/meminfo", [read]) of
	{error, _} -> {undefined, undefined};
	{ok, Dev} ->
	    Result = find_one_entry ("MemFree:", "~d kB", Dev),
	    file:close(Dev),
	    Result
    end.


% CPU speed.  This isn't a very good solution.  For one thing, it only
% reports the speed of the first CPU.  Also, CPU speeds can vary due
% to throttling and so on, so the real speed when you execute something
% may be different.

-spec cpu_speed () -> float() | undefined.
cpu_speed() ->
    case file:open("/proc/cpuinfo", [read]) of
	{error, _} -> undefined;
	{ok, Dev} ->
	    Result = find_one_entry ("cpu MHz"," :~f", Dev),  
	    %% Note that the spaces in the format actually skip all whitespace
	    file:close(Dev),
	    Result
    end.

-spec cpu_model_name () -> string() | undefined.
cpu_model_name() ->
    case file:open("/proc/cpuinfo", [read]) of
	{error, _} -> undefined;
	{ok, Dev} ->
	    Result = find_one_entry ("model name"," :~s", Dev),  
	    %% Note that the spaces in the format actually skip all whitespace
	    file:close(Dev),
	    Result
    end.

