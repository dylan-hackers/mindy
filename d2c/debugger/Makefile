debugger: exports.dylan dlfcn.dylan introspection.dylan debugger.lid
	d2c debugger.lid

dlfcn.dylan: dlfcn.intr
	melange --d2c -I `gcc --print-file-name=include` dlfcn.intr dlfcn.dylan, && mv dlfcn.dylan, dlfcn.dylan

clean:
	-rm *.c *.o *.mak debugger dlfcn.dylan *~

