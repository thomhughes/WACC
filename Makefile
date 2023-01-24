all:
	sbt compile assembly

clean:
	sbt clean && rm -rf wacc-42-compiler.jar

.PHONY: all clean
