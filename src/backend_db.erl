%%%-------------------------------------------------------------------
%%% @author cola
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. jul 2022 15:12
%%%-------------------------------------------------------------------
-module(backend_db).
-author("cola").
-include_lib("ebankIram/include/backend.hrl").
%% API
-export([create_db/0, lookup/2, lookup_by_name/2, new_account/4, credit/3, debit/3, is_pin_valid/3, all_accounts/1, close/1]).

create_db() ->
  ets:new(accounts, [set, {keypos, #account.acc_no}, named_table]).

lookup(AccountNumber, DBRef) ->
  case ets:lookup(DBRef, AccountNumber) of
    [] -> {error, DBRef};
    [_] -> ets:lookup(DBRef, AccountNumber)
  end.

lookup_by_name(Name, DBRef) ->
  ets:match(DBRef, #account{name = Name, _= '_'}).

new_account(AccountNumber, AccountPin, AccountName, DBRef) ->
  case ets:lookup(DBRef, AccountNumber) of
    [] -> ets:insert(DBRef, [#account{acc_no = AccountNumber, pin = AccountPin, name = AccountName}]);
    [_] -> {error, exists}
  end.

credit(AccountNumber, Amount, DBRef) ->
  case ets:lookup(DBRef, AccountNumber) of
    [] -> {error, DBRef};
    [_] -> AccountInfo = ets:lookup(DBRef, AccountNumber),
      [Data|Y] = AccountInfo,
      NewBalanceCredit = Data#account.balance + Amount,
      NewTransactionCredit = Data#account.transactions ++ [{credit, erlang:localtime(), Amount}],
      ets:update_element(accounts, 1, [{#account.balance, NewBalanceCredit}, {#account.transactions, NewTransactionCredit}])
  end.

debit(AccountNumber, Amount, DBRef) ->
  case ets:lookup(DBRef, AccountNumber) of
    [] -> {error, DBRef};
    [_] -> AccountInfo = ets:lookup(DBRef, AccountNumber),
      [Data|Y] = AccountInfo,
      NewBalanceCredit = Data#account.balance - Amount,
      NewTransactionCredit = Data#account.transactions ++ [{debit, erlang:localtime(), Amount}],
      ets:update_element(accounts, 1, [{#account.balance, NewBalanceCredit}, {#account.transactions, NewTransactionCredit}])
  end.

is_pin_valid(AccountNumber, Pin, DBRef) ->
  AccountInfo = ets:lookup(DBRef, AccountNumber),
  [Data|Y] = AccountInfo,
  AccountPin = Data#account.pin,
  string:equal(AccountPin, Pin).

all_accounts(DBRef) ->
  ets:tab2list(DBRef).

close(DBRef) ->
  ets:delete(DBRef).