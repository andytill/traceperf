# Benchmarks for erlang (beam) tracing

Work in progress benchmarks for erlang tracing, and methods of receiving traces e.g. receiving a trace message to a process or sending it directly to a port.

To run:

```shell
./traceperf.sh
```

Output:

```
type,            user,  sys,    cpu,   maxmem,     calls,   otp
idle,            0.12,  0.05,   27%,   24228kB,    500000,  20
tcp_port,        2.77,  2.28,   335%,  5014344kB,  500000,  20
file_port,       3.05,  15.93,  209%,  5066688kB,  500000,  20
local_process,   1.06,  0.51,   260%,  64892kB,    500000,  20
remote_process,  3.51,  0.28,   123%,  192832kB,   500000,  20
```
