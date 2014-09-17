#!/usr/bin/env bash
for Dir in $(find ./* -type d);
do
    FolderName=$(basename $Dir);
    if [ -f $Dir/Main.hs ]; then
        if [ -f $Dir/test.txt ]; then
            echo running $Dir
            runhaskell -Wall $Dir/Main.hs < $Dir/test.txt
        else
            echo No test.txt for $Dir
        fi
    fi
done