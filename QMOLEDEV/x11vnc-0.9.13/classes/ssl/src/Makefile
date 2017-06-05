JC_ARGS = -target 1.4 -source 1.4

all: clean
	@sh -c 'type javac'
	@sh -c 'type jar'
	@sh -c 'type jarsigner'
	cd ./tight || exit 1; javac $(JC_ARGS) *.java; jar cvf ../VncViewer.jar      *.class
	cd ./tight || exit 1; \
		rm -f *.class; \
		mv VncViewer.java VncViewer.java.SSL; \
		sed -e 's/disableSSL = false/disableSSL = true/' VncViewer.java.SSL > VncViewer.java; \
		javac $(JC_ARGS) *.java; jar cvf ../VncViewerNOSSL.jar *.class; \
		mv VncViewer.java.SSL VncViewer.java;
	cd ./ultra || exit 1; javac $(JC_ARGS) *.java; jar cvf ../UltraViewerSSL.jar *.class
	cp -p VncViewer.jar      SignedVncViewer.jar
	cp -p UltraViewerSSL.jar SignedUltraViewerSSL.jar
	jarsigner -keystore ./keystore0 -storepass abcdef SignedVncViewer.jar      swkey
	jarsigner -keystore ./keystore0 -storepass abcdef SignedUltraViewerSSL.jar swkey

update_jars: all
	cp -p VncViewer.jar SignedVncViewer.jar UltraViewerSSL.jar SignedUltraViewerSSL.jar ..
	cp -p VncViewerNOSSL.jar ../../VncViewer.jar

clean:
	rm -rf *.jar */*.class
