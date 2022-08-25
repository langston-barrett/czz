JAVA := $(wildcard *.java)

build: $(JAVA:.java=.class)

%.class: %.java
	javac -g $<

clean:
	rm -f *.class
