-module(xfp_xmerl).

-export([parse/2]).

-compile([export_all]).

-include_lib("xmerl/include/xmerl.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% xfp_xmerl:parse(Xml1, ["//Ad/Impressions/text()", "//Ad/AlternateText/text()", "//Ad/NavigateUrl/text()", "//Ad/ImageUrl/text()"]).
% xfp_xmerl:parse(Xml2, ["//Ad/@Impressions", "//Ad/@AlternateText", "//Ad/@NavigateUrl", "//Ad/@ImageUrl"]).

parse(Xml, XPath) ->
    try
        {Elements, []} = xmerl_scan:string(binary_to_list(Xml)),
        lists:flatten(lists:foldl(fun(XPath, Acc) -> [compose(xmerl_xpath:string(XPath, Elements), []) | Acc] end, [], XPath))
    catch
        C:R -> {C,R}
    end.

compose([], Acc) ->
    Acc;
compose([#xmlText{parents = [{Name, _} | Parents], value = Value} | Xs], Acc) ->
    compose(Xs, [{lists:foldl(fun({K, V}, Acc1) -> V + Acc1 end, 0, Parents), {Name, Value}} | Acc]);
compose([#xmlAttribute{name = Name, parents = Parents, value = Value} | Xs], Acc) ->
    compose(Xs, [{lists:foldl(fun({K, V}, Acc1) -> V + Acc1 end, 0, Parents), {Name, Value}} | Acc]).

