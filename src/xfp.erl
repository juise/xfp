-module(xfp).

%% API
-export([start/0,
         stop/0]).

-export([parse/2]).

-compile([export_all]).

-include_lib("erlsom/src/erlsom_sax.hrl").

-record(state, {idx         = 0,
                tag         = undefined,
                local_acc   = [],
                global_acc  = []
}).

%% ===================================================================
%% API functions
%% ===================================================================

start() ->
    application:ensure_started(xfp),
    ok.

stop() ->
    application:stop(xfp),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO:
% {name :: tag(), {need_value :: boolean(), [attr_name :: tag()]}}
% Use the record?
% Speed up attributes to event stream cycle

stat(N) ->
    {ok, Xml1} = file:read_file("/tmp/1.xml"),
    {ok, Xml2} = file:read_file("/tmp/2.xml"),
    F = fun(N, Fun) -> S = bear:get_statistics([begin {T, _} = timer:tc(Fun), T end || _ <- lists:seq(1, N)]), [proplists:lookup(K,S) || K <- [min, max, median]] end,

    R1 = F(100, fun() -> xfp:parse(Xml1, [{"ImageUrl", {true, []}}, {"NavigateUrl", {true, []}}, {"AlternateText", {true, []}}, {"Impressions", {true, []}}]) end),
    R2 = F(100, fun() -> xfp:parse(Xml2, [{"Ad", {false, ["ImageUrl", "NavigateUrl", "AlternateText", "Impressions"]}}]) end),
    R3 = F(100, fun() -> xfp_xmerl:parse(Xml1, ["//Ad/Impressions/text()", "//Ad/AlternateText/text()", "//Ad/NavigateUrl/text()", "//Ad/ImageUrl/text()"]) end),
    R4 = F(100, fun() -> xfp_xmerl:parse(Xml2, ["//Ad/@Impressions", "//Ad/@AlternateText", "//Ad/@NavigateUrl", "//Ad/@ImageUrl"]) end),
    io:format("R1 ~p~nR2 ~p~nR3 ~p~nR4 ~p~n", [R1, R2, R3, R4]).

% xfp:parse(Xml, [{"ImageUrl", {true, []}}, {"NavigateUrl", {true, []}}, {"AlternateText", {true, []}}, {"Impressions", {true, []}}]).
% xfp:parse(Xml1, [{"Ad", {true, ["ImageUrl", "NavigateUrl", "AlternateText", "Impressions"]}}]).

parse(Xml, Tags) ->
    {ok, State, _} = erlsom:parse_sax(Xml, #state{}, fun(Event, State) -> callback(Event, Tags, State) end, []),
    State.


callback({startElement, _Uri, Tag, _Prefix, Attributes}, [{Tag, {_, []}} | Tags], State) ->
    State#state{tag = Tag};

callback({startElement, _Uri, Tag, _Prefix, Attributes}, [{Tag, {_, Atrs}} | Tags], #state{idx = Idx} = State) ->
    Tags1 = atot(Atrs),
    State1 = lists:foldl(fun(Event, State) -> callback(Event, Tags1, State) end, State#state{idx = Idx + 1}, atoes(Attributes, [])),
    State1#state{tag = Tag};

callback({startElement, _Uri, _Tag, _Prefix, _Attributes} = Event, [Tag | Tags], State) ->
    callback(Event, Tags, State);

callback({startElement, _Uri, _Tag, _Prefix, _Attributes}, [], #state{idx = Idx} = State) ->
    State#state{idx = Idx + 1};


callback({endElement, _Uri, Tag, _Prefix}, [{Tag, {true, _}} | Tags], #state{tag = Tag, local_acc = LAcc, global_acc = GAcc, idx = Idx} = State) ->
    State#state{tag = undefined, local_acc = [], global_acc = [{Idx, Tag, lists:flatten(LAcc)} | GAcc]};

callback({endElement, _Uri, Tag, _Prefix}, [{Tag, {false, _}} | Tags], #state{tag = Tag, local_acc = LAcc} = State) ->
    State#state{tag = undefined, local_acc = []};

callback({endElement, _Uri, _Tag, _Prefix} = Event, [Tag | Tags], State) ->
    callback(Event, Tags, State);


callback({characters, Characters}, [{Tag, _} | Tags], #state{tag = Tag, local_acc = LAcc} = State) ->
    State#state{local_acc = [Characters | LAcc]};

callback({characters, _Characters} = Event, [Tag | Tags], State) ->
    callback(Event, Tags, State);


callback({ignorableWhitespace, Characters}, [{Tag, _} | Tags], #state{tag = Tag, local_acc = LAcc} = State) ->
    State#state{local_acc = [Characters | LAcc]};

callback({ignorableWhitespace, _Characters} = Event, [Tag | Tags], State) ->
    callback(Event, Tags, State);


callback(_Event, _Tags, State) ->
    State.


atoes([], Acc) ->
    Acc;
atoes([#attribute{localName=Name, value=Value} | Attributes], Acc) ->
    atoes(Attributes, [{startElement, [], Name, [], []}, {characters, Value}, {endElement, [], Name, []} | Acc]).

atot(Atrs) ->
    [{Atr, {true, []}} || Atr <- Atrs].

