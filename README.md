# Cellosaurus - A knowledge resources on cell lines

From the CALIPHO group of the SIB Swiss Institute of Bioinformatics

The Cellosaurus is a knowledge resource on cell lines. It attempt to describe all cell lines used in biomedical research.

# Cellosaurus syntax checker

This project is used to check the syntax of the cellosaurus.txt ASCII file and can also generate the OBO and XML format files.

## For developers

Build the jar

```
sbt assembly
```

Play with some classes

* Select a mainClass in build.sbt
* Run the class with a simila rommand as
```
sbt "run arg1 arg2 ... argN"
```


## For users 

```
java -jar jar-file cellosaurus-file
java -jar jar-file cellosaurus-file -obo (Also generates the OBO format file, cellosaurus.obo)
java -jar jar-file cellosaurus-file -xml (Also generates the XML format file, cellosaurus.xml)
java -jar jar-file cellosaurus-file -stats (Also generates some stats, directly on stdout)
java -jar jar-file cellosaurus-file conflicts=conflicts_file (Also creates a file with the cell lines with identical names/synonyms)
java -jar jar-file cellosaurus-file DRmap=mapfile (Also creates a file with all cross-references (DR) and publications (RX))
