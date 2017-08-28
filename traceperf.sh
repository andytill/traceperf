#!/bin/bash
erlc trace_perf.erl

num_calls=10000

trace_file=/tmp/trace-perf-log

## set up a remote node that can be contacted from the benchmark
erl -noshell \
    -name "remote_node@127.0.0.1" -setcookie trace_perf \
    -eval "trace_perf:be_remote_node()." &
remote_pid="$!"

# echo "Started remote node"
# echo "`ps -p ${remote_pid}`"

results_file="results.csv.tmp"

## csv headers, each row is printed by /usr/bin/time
echo "type, user, sys, cpu, maxmem, calls" > "${results_file}"

for trace_type in tcp_port file_port local_process remote_process; do
    if [ -f "${trace_file}" ]
    then
        # echo "Deleting File `ls -l \"${trace_file}\"`"
        rm "${trace_file}"
    fi
    erl_snippet="trace_perf:start(${trace_type}, ${num_calls})."
    # echo ${erl_snippet}
    /usr/bin/time --append -o "${results_file}" \
        --format "${trace_type}, %U, %S, %P, %MkB, ${num_calls}" \
        erl -noshell \
            -name trace_perf@127.0.0.1 -setcookie trace_perf \
            -eval "${erl_snippet}"
    # strace -c -S calls \
    #     erl -noshell \
    #         -name trace_perf@127.0.0.1 -setcookie trace_perf \
    #         -eval "${erl_snippet}"
done

## format the results into a new file
column -t ${results_file} > "results.csv"
rm "${results_file}"
cat "results.csv"

## kill may output a log that it has killed the remote
## node which is normal
kill -9 "${remote_pid}"