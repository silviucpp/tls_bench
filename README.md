tls_bench
================

A framework for load testing the Erlang TLS and TCP libs

Included libraries:

- [etls][1]
- [p1_tls][2]
- [fast_tls][3]
- [ssl][4]
- [gen_tcp][5]

Quick start
-----------

Getting all deps and compile:

```
rebar get-deps
rebar compile
```

Benchmark:

```bash
erl -pa ebin -deps deps -config test/sys.config +P 1000000
```

then run:

```
ssl_client:benchmark(ssl, 4000, 120, 120000, 10240).
```

How is working
-----------

```erlang 
benchmark(ClientMod, Port, ConcurrentConnections, Requests, MessageLength)
```

When `tls_bench` app is started it's starting an echo server for each library listed above (see `tets/sys.config`).

Then it's connecting a number of `ConcurrentConnections` clients that will send a number of `Requests` messages, each one 
having a length of `MessageLength`.

- `ClientMod` represents what library to be used for client connections (one of the above).
- `Port` is the port of the server that should receive the requests (for example if `etls` is on port 7000 you are using 7000 to bench 
 `etls`)

Tunnings
----------- 

You need to tune your kernel at least to increase the number of file descriptors that can be opened and the number of 
connections in the accept queue.

##### Mac OS

For `Mac OS` you can create the `/etc/sysctl.conf` file (by default doesn't exist) with the following settings :
  
```sh
# Nice articles explining fine tuning for BSD kernel:
# - https://calomel.org/freebsd_network_tuning.html

xkern.maxproc=2048
kern.maxprocperuid=2048
kern.maxfilesperproc=300000
kern.maxfiles=300000

# increase the number of sockets allowed in the accept queue
# use "netstat -Lan" to watch the queue

kern.ipc.soacceptqueue=30000
kern.ipc.somaxconn=30000

# kern.ipc.maxsockbuf is the maximum amount of memory, in bytes, which can be allocated 
# to a single socket. "netstat -m" displays the amount of network
# buffers used. Increase kern.ipc.maxsockbuf only if the counters for 
# "mbufs denied" or "mbufs delayed" are greater than zero(0).

kern.ipc.maxsockbuf=4194304
kern.ipc.nmbclusters=32768

# increase the port range

net.inet.ip.portrange.randomized=0
net.inet.ip.portrange.first=1024
net.inet.ip.portrange.last=65535
``` 

Notes
----------- 
 
- `etls` doesn't support the `backlog` option. It's using a default one which is too low so on my box the lib it's starting
to timeout if I use a big concurrency level. I had to patch `etls` to increase the backlog.
- `etls` doesn't support the `ciphers` tls option. So there is no way to limit the ciphers that are supported server side.

[1]:https://github.com/kzemek/etls
[2]:https://github.com/processone/tls
[3]:https://github.com/processone/fast_tls/
[4]:http://erlang.org/doc/man/ssl.html
[5]:http://erlang.org/doc/man/gen_server.html