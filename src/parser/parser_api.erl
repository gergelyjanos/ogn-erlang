-module(parser_api).

-export([parse_raw_line/1, parse_server_name/1]).

-spec parse_raw_line(Line) -> Result
   when
      Line :: binary(),
      Result :: ok.
parse_raw_line(Line) ->
   gen_server:cast(parser_server, {raw_line, Line}).

-spec parse_server_name(ServerName) -> Result
   when
      ServerName :: binary(),
      Result :: ok.
parse_server_name(ServerName) ->
   gen_server:cast(parser_server, {server_name, ServerName}).
