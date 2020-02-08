all: \
  ../cbits/rounded-common.inl \
  ../cbits/rounded-avx512.inl \
  ../src/FFIWrapper/Double.hs \
  ../src/FFIWrapper/Float.hs

../cbits/rounded-common.inl: gen-rounded-common.sh rounded-common.mustache c-double.yaml c-float.yaml
	./gen-rounded-common.sh

../cbits/rounded-avx512.inl: gen-rounded-avx512.sh rounded-avx512.mustache c-double.yaml c-float.yaml
	./gen-rounded-avx512.sh

../src/FFIWrapper/Double.hs ../src/FFIWrapper/Float.hs: gen-ffi-wrapper.sh FFIWrapper.mustache FFIWrapper-Double.yaml FFIWrapper-Float.yaml
	./gen-ffi-wrapper.sh

.PHONY: all
