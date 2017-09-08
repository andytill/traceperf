#!/bin/bash

## exit script on a non-zero return code
set -e

erlc traceperf.erl

num_calls=10000

trace_file=/tmp/trace-perf-log


KERL_DIR="$HOME/kerl"

## set up a remote node that can be contacted from the benchmark
erl -noshell \
    -name "remote_node@127.0.0.1" -setcookie traceperf \
    -eval "traceperf:be_remote_node()." &
remote_pid="$!"

# echo "Started remote node"
# echo "`ps -p ${remote_pid}`"

results_file="results.csv.tmp"

RUNS=1

## csv headers, each row is printed by /usr/bin/time
echo "type, elapsed, user, sys, cpu, maxmem, calls, otp" > "${results_file}"


for otp in "18.0"  "19.3"  "20.0"; do
    source ${KERL_DIR}/${otp}/activate
    otp_release=`erl -noshell -eval "io:format(erlang:system_info(otp_release)), erlang:halt(0)."`
for trace_type in idle tcp_port file_port local_process remote_process; do
for i in $(seq 1 $RUNS); do
    ## print a dot for every benchmark to show progress
    echo -n "."
    if [ -f "${trace_file}" ]
    then
        # echo "Deleting File `ls -l \"${trace_file}\"`"
        rm "${trace_file}"
    fi
    erl_snippet="traceperf:start(${trace_type}, ${num_calls})."
    # echo ${erl_snippet}
    /usr/bin/time --append -o "${results_file}" \
        --format "${trace_type}, %E, %U, %S, %P, %MkB, ${num_calls}, ${otp_release}" \
        erl -noshell \
            -name traceperf@127.0.0.1 -setcookie traceperf \
            -eval "${erl_snippet}"
    # strace -c -S calls \
    #     erl -noshell \
    #         -name traceperf@127.0.0.1 -setcookie traceperf \
    #         -eval "${erl_snippet}"
done ## number of runs
done ## tracer type
done ## otp version

## newline from the progress dots
echo ""

## format the results into a new file
column -t ${results_file} > "results.csv"
rm "${results_file}"
cat "results.csv"

## kill may output a log that it has killed the remote
## node which is normal
kill -9 "${remote_pid}"