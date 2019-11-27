all: yaps.so yaps_sync.so
	# Comment here preserves the prior tab
yaps.so: yaps.cpp
	Rscript --vanilla -e "TMB::compile('yaps.cpp','-O0 -g -safeunload=FALSE')"
yaps_sync.so: yaps_sync.cpp
	Rscript --vanilla -e "TMB::compile('yaps_sync.cpp','-O0 -g -safeunload=FALSE')"

clean:
	rm -rf *o