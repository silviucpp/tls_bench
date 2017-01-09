tls_bench
================

A framework for load testing the Erlang TLS and TCP libs

Included libraries:

- [etls][1]
- [p1_tls][2]
- [fast_tls][3]
- [ssl][4]
- [erltls][9]
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

You need to tune your kernel at least to increase the number of file descriptors that can be opened and the number of  connections in the accept queue.

In the testing we used the following 'sysctl.conf' files : 

- [Mac OS][6]  (by default doesn't exist so needs to be created in '/etc/sysctl.conf`)
- [Ubuntu 14.04][7]

Compile Erlang with BoringSSL
----------------------------

For compiling Erlang 19.1 with `boringssl` I had to write a [patch][8]. Also after compiling `boringssl` I merged `libdecrepit.a` with `libcrypto.a` running:

```sh
libtool -static -o libcrypto.a decrepit/libdecrepit.a crypto/libcrypto.a
```

Then I compiled `Erlang` as follow:

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

Benchmark
-----------

```
ssl_client:benchmark(ssl, EchoServerPort, 50, 80000, 30*1024).
```

##### On Mac OS:

Test was performed on :

```
OSX 10.12.1 MacBook Pro (Retina, 15-inch, Mid 2014) 
CPU: 2.5 GHz Intel Core i7, 
Memory: 16 GB 1600 MHz DDR3
Erlang version: 19.1
OpenSSL version: OpenSSL 1.0.2j  26 Sep 2016
```

Benchmark: (All results are in MB/s)


| cipher                    | erlang-boringssl | erlang-openssl  | p1_tls    | fasttls    | etls      | erltls   | 
|:-------------------------:|:----------------:|:---------------:|:---------:|:----------:|:---------:|:--------:|
|AES128-GCM-SHA256          | 723.45           | 683.16          | 761.89    | 745.74     | 413.94    |1.06 GB/s |														
|AES128-SHA					| 419.98	       | 409.95          | 385.60    | 390.19     | 280.31    |573.79	 |
|AES128-SHA256				| 308.74           | 323.80          | 242.97	 | 242.38	  | 248.77    |417.61	 |
|ECDHE-RSA-AES128-GCM-SHA256| 693.55           | 643.06          | 756.51    | 764.23     | 415.65    |1.09 GB/s |

Also I compiled `p1_tls` and `fast_tls` with `boringssl`. Results for `AES128-GCM-SHA256` cipher are:

- `p1_tls`   - > 764.81 MB/s
- `fast_tls` - > 766.10 MB/s

##### On Ubuntu 14.04:

```
Ubuntu 14.04
CPU: Intel(R) Core(TM) i5-2500 CPU @ 3.30GHz
Memory: 8 GB
Erlang version 19.1
OpenSSL version: OpenSSL 1.0.2g  1 Mar 2016
```

| cipher                    | erlang-boringssl | erlang-openssl  | p1_tls    | fasttls    |  etls     | erltls    |
|:-------------------------:|:----------------:|:---------------:|:---------:|:----------:|:---------:|:---------:|
|AES128-GCM-SHA256          | N/A              | 1230 (1.23 GB)  | 184.13    | 184.61     | N/A       |N/A		  |																	
|AES128-SHA					| N/A	           | 840.26          | 111.84    | 110.78     | N/A       |N/A		  |
|AES128-SHA256				| N/A              | 615.83          | 65.96	 | 66.28	  | N/A       |N/A		  |
|ECDHE-RSA-AES128-GCM-SHA256| N/A              | 1220 (1.22 GB)  | 180.42    | 181.21     | N/A       |N/A		  |

Notes:

- I didn't tested Erlang compiled with `BoringSSL`
- I didn't tested `etls` because requires a new compiler than the one available on Ubuntu 14.04
- As you already might notice my system is using an `openssl` version newer than the one that comes with the distribution
- It's very surprising that on Linux (I tested 3 different machines) the performances are so bad for `p1_tls` and
`fast_tls`. CPU it's 100 % on all cores but most probably problem is somewhere in how OpenSSL is used. I also tried them compiled with `BoringSSL` and results were the same. 
I suspected a problem with `gen_tcp` (both being based on this), but benchmarking `gen_tcp` had pretty good results, over 4.3 GB/s

Notes
----------- 
 
- `etls` is not supporting the `ciphers` tls option. So there is no way to limit the ciphers that are supported server side.
- Code for `etls` is disabled as time to compile the lib you require a lot of dependencies that are not available on some systems. To enable it uncomment:
    - in `rebar.config` the etls line where the dep is downloaded
    - add `etls` to `tls_bench.app.src` in the `applications` section
    - uncomment `ok = generic_server:start(?MOD_ETLS),` in `tls_bench_app.erl` 

[1]:https://github.com/kzemek/etls
[2]:https://github.com/processone/tls
[3]:https://github.com/processone/fast_tls/
[4]:http://erlang.org/doc/man/ssl.html
[5]:http://erlang.org/doc/man/gen_server.html
[6]:https://raw.githubusercontent.com/silviucpp/tls_bench/master/test/osx_sysctl.conf
[7]:https://raw.githubusercontent.com/silviucpp/tls_bench/master/test/ubuntu_sysctl.conf
[8]:https://github.com/silviucpp/tls_bench/blob/master/test/boringssl.patch
[9]:https://github.com/silviucpp/erltls