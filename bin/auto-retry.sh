#!/bin/sh

if [ "$#" -eq 0 ]; then
    echo "Usage: $0 <command> [args...]"
    exit 1
fi

while true; do
    clear
    "$@"
    status=$?
    if [ "$status" -eq 0 ]; then
        echo "Command '$*' succeeded. Exiting loop."
        exit 0
    else
        echo "Command '$*' failed with exit code $status. Restarting..."
        sleep 1
    fi
done
