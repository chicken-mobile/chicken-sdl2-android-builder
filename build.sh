#!/bin/bash

function reown {
 [ -n "${HOST_USER_ID}" ] && chown ${HOST_USER_ID} -R .
}

ndk-build && \
    chicken-copy-libs && \
    ant debug && \
    reown
