scp target/cello-parser-0.1-jar-with-dependencies.jar npteam@kant:/share/sib/calipho/calipho/cellosaurus/celloparser.jar
scp src/main/scala/org/nextprot/parser/cellosaurus/CelloParser.scala npteam@kant:/share/sib/calipho/calipho/cellosaurus/sources
ssh npteam@kant "cd /share/sib/calipho/calipho/cellosaurus/; mv cellosaurus.xsd cellosaurus.xsd.$(date +'%Y-%m-%d-%H%M') "
scp ./cellosaurus.xsd npteam@kant:/share/sib/calipho/calipho/cellosaurus/

