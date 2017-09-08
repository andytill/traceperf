# Benchmarks for erlang (beam) tracing

Work in progress benchmarks for erlang tracing, and methods of receiving traces e.g. receiving a trace message to a process or sending it directly to a port.

### Running

```shell
./traceperf.sh
```

### Output

```
type,            elapsed,  user,  sys,   cpu,   maxmem,     calls,  otp
idle,            0:00.69,  0.08,  0.10,  26%,   22748kB,    10000,  18
tcp_port,        0:00.83,  0.39,  0.74,  136%,  802612kB,   10000,  18
file_port,       0:00.63,  0.54,  0.43,  155%,  743960kB,   10000,  18
local_process,   0:03.71,  2.90,  2.12,  135%,  1932020kB,  10000,  18
remote_process,  0:04.42,  3.43,  1.63,  114%,  1782040kB,  10000,  18
idle,            0:00.66,  0.13,  0.04,  26%,   23656kB,    10000,  19
tcp_port,        0:00.44,  0.40,  0.15,  126%,  91868kB,    10000,  19
file_port,       0:00.42,  0.40,  0.22,  147%,  76840kB,    10000,  19
local_process,   0:01.93,  1.79,  0.98,  143%,  1117140kB,  10000,  19
remote_process,  0:04.97,  3.29,  2.94,  125%,  1744304kB,  10000,  19
idle,            0:00.65,  0.10,  0.07,  26%,   19868kB,    10000,  20
tcp_port,        0:00.42,  0.37,  0.14,  122%,  82120kB,    10000,  20
file_port,       0:00.41,  0.38,  0.17,  135%,  72716kB,    10000,  20
local_process,   0:02.14,  2.04,  0.77,  131%,  1908484kB,  10000,  20
remote_process,  0:01.90,  1.70,  0.59,  120%,  1100272kB,  10000,  20
```

### Dependencies

Kerl is required to run the benchmarks. The `kerl` command must be on the path and releases to be installed in the `~/kerl/` directory. Check the shell script for which releases are tested, and must be installed or deleted from the list of releases.
