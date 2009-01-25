-module (element_gravatar).
-compile(export_all).
-include ("wf.inc").

reflect() -> record_info(fields, gravatar).

render(ControlID, Record) -> 
    GravatarEmail = Record#gravatar.email,
    Image = #image {
        image = gravatar_icon(GravatarEmail)
    },
    element_image:render(ControlID, Image).

gravatar_icon(GravatarEmail) ->
    GravatarId = digest2str(erlang:md5(GravatarEmail)),
    wf:f("http://www.gravatar.com/avatar.php?size=32&gravatar_id=~s" ,[GravatarId]).

digest2str(Digest) ->
    [[nibble2hex(X bsr 4), nibble2hex(X band 15)] ||
    X <- binary_to_list(Digest)].

-define(IN(X,Min,Max), X >= Min, X =< Max).
nibble2hex(X) when ?IN(X, 0, 9)   -> X + $0;
nibble2hex(X) when ?IN(X, 10, 15) -> X - 10 + $a.
