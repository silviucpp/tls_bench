REBAR=rebar

export CPPFLAGS=-I/usr/local/opt/openssl/include/
export CFLAGS=-I/usr/local/opt/openssl/include/
export LDFLAGS=-L/usr/local/opt/openssl/lib

compile:
	${REBAR} compile

clean:
	${REBAR} clean
