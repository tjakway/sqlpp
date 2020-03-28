#!/bin/sh

dl_if_not_found() {
    DL_IF_NOT_FOUND_USAGE="dl_if_not_found [dest] [url] [optional SHA-256 hash]"
    if [ $# -lt 1 ] || [ $# -gt 3 ]; then
        echo "$DL_IF_NOT_FOUND_USAGE" 1>&2
        exit 1
    elif [ $# -ge 2 ]; then
        DL_IF_NOT_FOUND_DEST="$1"
        DL_IF_NOT_FOUND_URL="$2"
        if [ $# -eq 3 ]; then
            DL_IF_NOT_FOUND_TARGET_HASH="$3"
        else
            DL_IF_NOT_FOUND_TARGET_HASH="NULL"
        fi
    fi

    wget --continue \
        -O "$DL_IF_NOT_FOUND_DEST" \
        "$DL_IF_NOT_FOUND_URL"

    DL_IF_NOT_FOUND_WGET_RES="$?"
    if [ "$DL_IF_NOT_FOUND_WGET_RES" = "0" ]; then
        #TODO: use shasum's built-in checking behavior
        if [ "$DL_IF_NOT_FOUND_TARGET_HASH" != "NULL" ]; then
            DL_IF_NOT_FOUND_SHASUM_RES=$( \
                shasum -a 256 "$DL_IF_NOT_FOUND_DEST" | \
                awk '{print $1}')

            #make sure the shasum invocation succeeded
            DL_IF_NOT_FOUND_SHASUM_EXIT_CODE="$?"
            if [ "$DL_IF_NOT_FOUND_SHASUM_EXIT_CODE" != "0" ]; then
                echo "shasum -a 256 $DL_IF_NOT_FOUND_DEST " \
                    "failed with exit code $DL_IF_NOT_FOUND_SHASUM_EXIT_CODE" \
                    1>&2
                exit 1
            fi

            #verify the hashes
            if [ "$DL_IF_NOT_FOUND_SHASUM_RES" = "$DL_IF_NOT_FOUND_TARGET_HASH" ]; then
                #hashes matched, return success
                exit 0
            else
                echo "Hashes did not match; " \
                    "expected $DL_IF_NOT_FOUND_TARGET_HASH, but got " \
                    "$DL_IF_NOT_FOUND_SHASUM_RES" 1>&2
                exit 1
            fi
        fi
    else
        exit "$DL_IF_NOT_FOUND_WGET_RES"
    fi
}

dl_if_not_found "$@"
