INSTALL_PATH = /usr/local

C_SOURCES = jpgf.c jni_utils.c
JAVA_SOURCES = $(wildcard org/grammaticalframework/pgf/*.java)

JNI_INCLUDES = $(if $(wildcard /usr/lib/jvm/default-java/include/.*), -I/usr/lib/jvm/default-java/include -I/usr/lib/jvm/default-java/include/linux, \
               $(if $(wildcard /usr/lib/jvm/java-1.11.0-openjdk-amd64/include/.*), -I/usr/lib/jvm/java-1.11.0-openjdk-amd64/include/ -I/usr/lib/jvm/java-1.11.0-openjdk-amd64/include/linux, \
               $(if $(wildcard /System/Library/Frameworks/JavaVM.framework/Versions/A/Headers/.*), -I/System/Library/Frameworks/JavaVM.framework/Versions/A/Headers, \
               $(if $(wildcard /Library/Java/Home/include/.*), -I/Library/Java/Home/include/ -I/Library/Java/Home/include/darwin, \
               $(error No JNI headers found)))))

# For compilation on Windows replace the previous line with something like this:
#
# JNI_INCLUDES  = -I "C:/Program Files/Java/jdk1.8.0_171/include" -I "C:/Program Files/Java/jdk1.8.0_171/include/win32" -I "C:/MinGW/msys/1.0/local/include"
# WINDOWS_LDFLAGS = -L"C:/MinGW/msys/1.0/local/lib" -no-undefined

GCC = gcc
LIBTOOL = $(if $(shell command -v glibtool 2>/dev/null), glibtool, libtool)  --tag=CC

# For cross-compilation from Linux to Windows replace the previous two lines with:
#
# GCC = x86_64-w64-mingw32-gcc
# LIBTOOL = ../c/libtool
# WINDOWS_CCFLAGS = -I$(INSTALL_PATH)/include
# WINDOWS_LDFLAGS = -L$(INSTALL_PATH)/lib -no-undefined

all: libjpgf.la jpgf.jar

libjpgf.la: $(patsubst %.c, %.lo, $(C_SOURCES))
	$(LIBTOOL) --mode=link $(GCC) $(CFLAGS) -g -O -o libjpgf.la -shared $^ -rpath $(INSTALL_PATH)/lib -lgu -lpgf $(WINDOWS_LDFLAGS)

%.lo : %.c
	$(LIBTOOL) --mode=compile $(GCC) $(CFLAGS) -g -O -c $(JNI_INCLUDES) $(WINDOWS_CCFLAGS) -std=c99 -shared $< -o $@

jpgf.jar: $(patsubst %.java, %.class, $(JAVA_SOURCES))
	jar -cf $@ org/grammaticalframework/pgf/*.class

%.class : %.java
	javac $<

install: libjpgf.la jpgf.jar
	$(LIBTOOL) --mode=install install -s libjpgf.la $(INSTALL_PATH)/lib
	install jpgf.jar $(INSTALL_PATH)/lib


doc:
	javadoc org.grammaticalframework.pgf -d java-api

clean:
	rm -f *.lo
	rm -f *.la
	rm -f -r .libs
	rm -f *.jar
	rm -f org/grammaticalframework/*/*.class
