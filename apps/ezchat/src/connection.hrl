%%%---------------------------------------------------------------------------
%%% @doc Messaging record definitions
%%% @end
%%%---------------------------------------------------------------------------

-author('Lee Sylvester <lee.sylvester@gmail.com>').

-record(messaging_user,{pid,username,room,bytes=0,datetime}).