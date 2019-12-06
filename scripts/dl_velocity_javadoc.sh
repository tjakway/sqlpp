#!/bin/sh

VELOCITY_JAVADOC_DEST='velocity-engine-core-2.1-javadoc.jar'
VELOCITY_JAVADOC_URL='http://us.mirrors.quenda.co/apache//velocity/engine/2.1/velocity-engine-core-2.1-javadoc.jar'
VELOCITY_JAVADOC_SHA256='b645c523310699679f4d0b5f4fc889e9b48ad7abae40544ecff5351416ba3805'

./dl_if_not_found.sh \
    "$VELOCITY_JAVADOC_DEST" \
    "$VELOCITY_JAVADOC_URL" \
    "$VELOCITY_JAVADOC_SHA256"
