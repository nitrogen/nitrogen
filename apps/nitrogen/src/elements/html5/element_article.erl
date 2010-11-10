% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module (element_article).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, article).

render_element(Record) ->
    wf_tags:emit_tag(article, Record#article.body, [
        {class, ["article", Record#article.class]},
        {style, Record#article.style}
    ]).
