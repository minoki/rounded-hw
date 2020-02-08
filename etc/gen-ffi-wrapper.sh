#!/bin/bash
mustache=$(which mustache mustache-2.6)
$mustache FFIWrapper-Double.yaml FFIWrapper.mustache > ../src/FFIWrapper/Double.hs
$mustache FFIWrapper-Float.yaml FFIWrapper.mustache > ../src/FFIWrapper/Float.hs
