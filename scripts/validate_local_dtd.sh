#!/bin/sh

#see https://stackoverflow.com/questions/29072962/xmllint-how-to-validate-an-xml-using-a-local-dtd-file

USAGE="validate_local_dtd.sh [local_dtd] [xml_file]"

if [ $# -ne 2 ]; then
    echo "$USAGE" 1>&2
    exit 1
else
    LOCAL_DTD="$1"
    LOCAL_XML="$2"
fi

xmllint --noout --dtdvalid "$LOCAL_DTD" "$LOCAL_XML"
