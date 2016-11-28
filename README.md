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

Results
-----------

Test was performed on :

```
OSX 10.12.1 MacBook Pro (Retina, 15-inch, Mid 2014) 
CPU: 2.5 GHz Intel Core i7, 
Memory: 16 GB 1600 MHz DDR3
```

For compiling Erlang 19.1 with boring ssl I had to apply the `test/boringssl.patch`

Also after compiling `boringssl` I merged `libdecrepit.a` with `libcrypto.a` running:

```sh
libtool -static -o libcrypto.a decrepit/libdecrepit.a crypto/libcrypto.a
```

Then I compiled erlang as follow:

```sh
./otp_build autoconf
./configure --prefix=/usr/local/erlang-boringssl/ --disable-dynamic-ssl-lib --with-ssl={path_to_boring_ssl}
export MAKEFLAGS=-j8
make
sudo make install
```

Erlang 19.1 with boringssl:

```
crypto:info_lib() => [{<<"OpenSSL">>,268443823,<<"BoringSSL">>}]
```

Erlang 19.1 without boringssl:

```
crypto:info_lib() => [{<<"OpenSSL">>,268443807, <<"OpenSSL 1.0.2j  26 Sep 2016">>}]
```

Benchmark: (All results are in MB/s)

```
ssl_client:benchmark(ssl, EchoServerPort, 50, 80000, 30*1024).
```

| cipher                    | erlang-boringssl | erlang-openssl  | p1_tls         | fasttls        |      etls     |
|:-------------------------:|:----------------:|:---------------:|:--------------:|:---------------|--------------:|
|AES128-GCM-SHA256          | 723.45           | 683.16          | 761.89         | 745.74         | 413.94        |																	
|AES128-SHA					| 419.98	       | 409.95          | 385.60         |	390.19         | 280.31        |
|AES128-SHA256				| 308.74           | 323.80          | 242.97	      | 242.38	       | 248.77        |
|ECDHE-RSA-AES128-GCM-SHA256| 693.55           | 643.06          | 756.51         |	764.23         | 415.65        |

Also I compiled `p1_tls` and `fast_tls` with `boringssl`. Results for `AES128-GCM-SHA256` cipher are:

- `p1_tls` - > 764.81 MB/s
- `fast_tls` - > 766.10 MB/s

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