-ifndef(MLLIB_DEBUG).
-define(MLLIB_DEBUG, ok).

-ifdef(DEBUG).
-define(LOG(Message, Args), io:format("[DEBUG] " ++ Message, Args)).
%-define(LOG(Message), io:format("[DEBUG] " ++ Message)).
-else.
-define(LOG(X, Y), pass).
%-define(LOG(X), pass).
-endif.

-ifdef(INFO).
-define(INFO(Message, Args), io:format("[INFO] " ++ Message, Args)).
-else.
-define(INFO(X, Y), pass).
-endif.

-ifdef(DETAILS).
-define(DETAIL(Message, Args), io:format("[DETAIL] " ++ Message, Args)).
-else.
-define(DETAIL(X, Y), pass).
-endif.


-endif.


