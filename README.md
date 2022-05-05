# IBMiTools

Some usefull IBMi tools.
If you have any new idea just let me know.

## Tools:

### Tool0001

**Generate a list of job descriptions whose library list contains one or all of the given libraries.**

Case: You would like to delete a library and you should be check that this library is not used in any job description.

Sources:
- Tool0001.cmd
- Tool0001.rpgle

### Tool0002

**Generate a list of QMQRY files containing a text.**

Case: You have an sql object (user defined function, table, view, ...) and you would like to know in which Query Manager Query file (*QMQRY) is used. 

Sources:
- Tool0002.cmd
- Tool0002.sqlrpgle

### Tool0003

**Convert IFS SQL stream file to QMQRY File.**

Case: You have an IFS SQL stream file and you would like to use it with STRQMQRY.

Sources:
- Tool0003.cmd
- Tool0003.sqlrpgle

### Tool0004

**Generate a list of a physical file dependencies with key field list information.**

Case: The Index Advisor reports - because of some SQL stamements - that you should create a new index for a table.
You know that this table has already a lot of indexes, and you would like to check if you could find a logical file
which could be used for this optimalisation.

Sources:
- Tool0004.cmd
- Tool0004.rpgle

## Coming soon...

- Generate a list of physical file members containing a text
- Generate a list of diabled NetServer users
- Enable NetServer user

