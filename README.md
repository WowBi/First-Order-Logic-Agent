# First-Order-Logic-Agent

This is a First Order Logic Agent to determine the truth of queries based on Knowledge Base you give.

The program take no command-line arguments. It reads a text file called “input.txt” in the current directory that contains a problem definition and write a file “output.txt” with the solution.

To parse input strings, I used Python Ply module.
To learn more about Ply: http://www.dabeaz.com/ply/ply.html


For test case:

```
input.txt:
2
Ancestor(Liz,Billy)
Ancestor(Liz,Bob)
6
Mother(Liz,Charley)
Father(Charley,Billy)
((~Mother(x,y)) | Parent(x,y))
((~Father(x,y)) | Parent(x,y))
((~Parent(x,y)) | Ancestor(x,y))
((~(Parent(x,y) & Ancestor(y,z))) | Ancestor(x,z))

output.txt:
TRUE
FALSE

```

