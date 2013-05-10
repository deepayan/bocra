
all:
	bash createProject.sh
	make -f Makefile.bocra
	if test -e libbocra.so; then cp libbocra.so bocra.so; fi
	## for macx:
	if test -e libbocra.dylib; then cp libbocra.dylib bocra.so; fi


clean:
	rm -f *.so *.o *.a libqt* bocra.pro Makefile.bocra moc*.cpp


