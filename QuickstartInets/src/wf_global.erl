-module (wf_global).
-export ([
	sign_key/0,
	session_timeout/0,
	request/1
]).


sign_key() -> "Replace This With A Unique Key".
session_timeout() -> 20.
request(_) -> ok.
