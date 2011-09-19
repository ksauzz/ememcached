Erlang Memcached Clone for study
================================

build
-----

::

  make compile

test running
------------

::

  erl -pa ebin

  Eshell V5.5.5 (abort with ^G)
  1> application:start(ememcached).
  ok.

release build
-------------

create application and module

::

  mkdir rel
  cd rel
  ../rebar create-app appid=ememcached
  ../rebar create template=simplemod modid=ememcached
