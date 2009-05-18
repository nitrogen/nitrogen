-module (wf_context).
-include ("wf.inc").
-export ([
	make_context/3
]).

make_context(Module, PathInfo, RequestBridge) ->
	Context = #context {
		page_module = Module,
		path_info = PathInfo,
		request = RequestBridge
	},
	
	% Init the page state...
	PageState = default_page_state_bridge:new([]),
	PageState1 = PageState:init(Context),
	
	% Init the session state...
	SessionState = default_session_state_bridge:new([]),
	SessionState1 = SessionState:init(Context),
	
	% Init the cookies...
	CookieBridge = default_cookie_bridge:new([]),
	CookieBridge1 = CookieBridge:init(Context),
	
	% Init the cache...
	CacheBridge = default_cache_bridge:new([]),
	CacheBridge1 = CacheBridge:init(Context),
	
	% Init security...
	SecurityBridge = default_security_bridge:new([]),
	SecurityBridge1 = SecurityBridge:init(Context),
	
	% Init log...
	LogBridge = default_log_bridge:new([]),
	LogBridge1 = LogBridge:init(Context),
	
	% Update the context...
	Context#context {
		page_state_bridge=PageState1,
		session_state_bridge=SessionState1,
		cookie_bridge=CookieBridge1,
		cache_bridge=CacheBridge1,
		security_bridge=SecurityBridge1,
		log_bridge=LogBridge1
	}.
	
	
	
	