%%%-------------------------------------------------------------------
%%% @author cola
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. jul 2022 16:22
%%%-------------------------------------------------------------------
-module(backend).
-author("cola").
-behavior(gen_server).
%% API
-export([init/1, handle_call/3, handle_cast/2]).
-include_lib("backend.hrl").

start_link() ->
  gen_server:start_link({local, backend}, backend, [], []).

stop() ->
  gen_server:cast(backend, stop).

init(Args) ->
  backend_db:create_db(),
  {ok, Accounts} = file:consult(code:priv_dir(ebankIram ++ "/accounts.txt")),
  lists:foreach(fun(Element) -> backend_db:new_account(element(1, Element), element(3, Element), element(4, Element), accounts) end, Accounts),
  lists:foreach(fun(Element) -> backend_db:credit(element(1, Element), element(2, Element), accounts) end, Accounts).

handle_call(Request, From, State) ->
  erlang:error(not_implemented).

handle_cast(stop, LoopData) ->
  {stop, normal, LoopData}.

terminate(_Reason, _LoopDate) ->
  backend_db:close(accounts).

account(AccountNumber) ->
  case backend_db:lookup(AccountNumber, accounts) of
    [] -> {error, "No accounts for the account number"};
    [_] -> [Data|Y] = backend_db:lookup(AccountNumber, accounts),
      Data#account
  end.

accounts_by_name(Name) ->
  backend_db:lookup_by_name(Name, accounts).

list_accounts() ->
  backend_db:all_accounts(accounts).

pin_valid(AccountNumber, Pin) ->
  backend_db:is_pin_valid(AccountNumber, Pin, accounts).

withdraw(AccountNumber, Pin, Amount) ->
  withdraw(pin_valid(AccountNumber ,Pin), AccountNumber, Pin, Amount).
withdraw(true, AccountNumber, Pin, Amount) ->
  check_balance_and_withdraw(check_enough_balance(AccountNumber, Amount), AccountNumber, Amount);
withdraw(_, AccountNumber, Pin, Amount) ->
  {error, "Incorrect Pin brother"}.

check_balance_and_withdraw(true, AccountNumber, Amount) ->
  backend_db:debit(AccountNumber, Amount, accounts);
check_balance_and_withdraw(_, AccountNumber, Amount) ->
  {error, "Not enough minerals"}.

deposit(AccountNumber, Amount) ->
  backend_db:credit(AccountNumber, Amount, accounts),
  {ok, "Correct deposit."}.

transfer(AccountNumberFrom, AccountNumberTo, Amount, PinFrom) ->
  transfer(pin_valid(AccountNumberFrom, PinFrom), AccountNumberFrom, AccountNumberTo, Amount, PinFrom).
transfer(true, AccountNumberFrom, AccountNumberTo, Amount, PinFrom) ->
  check_balance_transfer(check_enough_balance(AccountNumberFrom, Amount),AccountNumberFrom, AccountNumberTo, Amount);
transfer(_, AccountNumberFrom, AccountNumberTo, Amount, PinFrom) ->
  {error, "Incorrect Pin brother"}.

check_balance_transfer(true, AccountNumberFrom, AccountNumberTo, Amount) ->
  backend_db:debit(AccountNumberFrom, Amount, accounts),
  backend_db:debit(AccountNumberTo, Amount, accounts);
check_balance_transfer(_, AccountNumberFrom, AccountNumberTo, Amount) ->
  {error, "Not enough minerals"}.

balance(AccountNumber, Pin) ->
  balance(pin_valid(AccountNumber, Pin), AccountNumber, Pin).
balance(true, AccountNumber, Pin) ->
  [Data|Y] = backend_db:lookup(AccountNumber, accounts),
  Data#account.balance;
balance(_, AccountNumber, Pin) ->
  {error, "Incorrect Pin"}.

transactions(AccountNumber, Pin) ->
  transactions(pin_valid(AccountNumber, Pin), AccountNumber, Pin).

transactions(true, AccountNumber, Pin) ->
  [Data|Y] = backend_db:lookup(AccountNumber, accounts),
  Data#account.transactions;
transactions(_, AccountNumber, Pin) ->
  {error, "Incorrect Pin"}.

check_enough_balance(AccountNumber, Amount) ->
  [Data|Y] = backend_db:lookup(AccountNumber, accounts),
  if
    Data#account.balance - Amount < 0 -> false;
    Data#account.balance - Amount >= 0 -> true
  end.



