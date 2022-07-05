%%%-------------------------------------------------------------------
%%% @author cola
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. jul 2022 15:10
%%%-------------------------------------------------------------------
-author("cola").
-record(account, {acc_no, balance = 0, pin, name, blocked = false, transactions = []}).