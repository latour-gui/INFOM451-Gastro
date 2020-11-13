# INFOM451 Gastro

This sbt scala project was created with the intellij IDEA.

The purpose of this project is to submit some application of the scala theory lessons in the context of a fun situation asking us to produce some kind of menu creator that has to propose healthy meals.

I chose to use another dataset found [here](http://www.afsca.be/denreesalimentaires/complementsalimentaires/_documents/2009-06-16_Circ_nouvelles_valeurs_AJR_ext-1_000.pdf) to get some guidance of what is a good meal and what's not.

### Disclaimer

Of course, you should not trust to the results of this program for your real menu.
The only purpose is to write idiomatic scala, not to provide real dietary advices.   


### Find the objectives

The homework directive clearly states that some objectives must be covered by the project, and the student must point out the said objective in his code.
Since I took the liberty to split the project into multiple files, the search of such objectives could be tedious.

Here is a simple command that will help the corrector to easily get all the said objectives:

```shell script
grep -rn --color -A 5 "// objective :"
``` 