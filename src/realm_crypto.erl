-module(realm_crypto).
-compile(export_all).
-include("realm_records.hrl").
-define(I, /unsigned-integer).
-define(IB, /unsigned-integer-big).
-define(IL, /unsigned-integer--little).
-define(K, 20).

encrypt(Header, Key) ->
    encrypt(Header, Key, <<>>).
encrypt(<<>>, Key, Result) -> {Result, Key};
encrypt(<<OldByte:8?I, Header/binary>>, #crypt_state{si = SI, sj = SJ, key = K}, Result) ->
    NewByte = lists:nth(SJ+1, K) bxor OldByte + SI,
    NewSJ   = (SJ + 1) rem ?K,
    encrypt(Header, #crypt_state{si  = NewByte, sj  = NewSJ, key = K}, <<Result/binary, NewByte:8>>).

decrypt(Header, Key) ->
    decrypt(Header, Key, <<>>).
decrypt(<<>>, Key, Result) -> {Result, Key};
decrypt(<<OldByte:8?I, Header/binary>>, #crypt_state{ri = RI, rj = RJ, key = K}, Result) ->
    NewByte = lists:nth(RJ+1, K) bxor (OldByte - RI),
    NewRJ   = (RJ + 1) rem ?K,
    encrypt(Header, #crypt_state{ri  = OldByte, rj  = NewRJ, key = K}, <<Result/binary, NewByte:8>>).

encryption_key(A) ->
    [{_, K}] = ets:lookup(connected_clients, A),
    Seed = 16#38A78315F8922530719867B18C04E2AA,
    Chu1 = 16#36363636363636363636363636363636,
    Chu2 = 16#5C5C5C5C5C5C5C5C5C5C5C5C5C5C5C5C,
    Sha1 = crypto:sha(<<(Seed bxor Chu1):128?IB, Chu1:128, Chu1:128, Chu1:128, K/binary>>),
    binary_to_list(crypto:sha(<<(Seed bxor Chu2):128?IB, Chu2:128, Chu2:128, Chu2:128, Sha1/binary>>)).