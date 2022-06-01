# IBMiTools

Some usefull IBMi tools.
If you have any new idea just let me know.

## Tools

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

### Tool0005

**Write in a log file which program is opening a database file.**

Case: You have a database file. You know (or not) that sometimes somebody is using this database file (may be just opening without any read) but you don't know who and when.
- Trigger doesn't help because there is no record operation (for example the database file is empty)
- Journal doesn't help because the file is already journalled for other cases (and the journals are managed/deleted automatically)

Sources:
- Tool0005.sqlrpgle

Steps:
1. Create a config data area (Positions 1-10: Library, 11-20: Filename).
2. Create the log file (qgpl.tool0005log).
3. Compile the Tool0005 program.
4. Add exit program (QIBM_QDB_OPEN).

### Tool0006

**Generate a list of diabled NetServer users.**

Case: You would like get a list about the disabled NetServer users (without Navigator for i).
You can get the list into a spool file or in a physical file.

Sources:
- Tool0006.cmd
- Tool0006.rpgle

### Tool0007

**Enable disabled NetServer user.**

Case: You would like to enable a disabled NetServer user (without Navigator for i).

Sources:
- Tool0007.cmd
- Tool0007.rpgle

### Tool0008a, Tool0008b

**Collect information about FTP activities.**

Case: You would like to know (and collect into a table) who and wenn is loggin into the system with ftp
and what he/she is doing with which files/folders.

Sources:
- Tool0008a.sqlrpgle
- Tool0008b.sqlrpgle

### Tool0009

**Generate a list of physical file members containing a text.**

Case: You have a physical file with a lot of members containing stream text content (for example EDI files in EDIFACT format).
You would like to search for text in the file but because the stream is 'splitted' to fix record length, it can happen,
that the text you must find is splitted to in two records. And you will not to lose this occurrances.

Features:
- Output: display (F12: skip this member, F3: exit the search) or print.
- Sort of members by creation date (ascendings or descendings) to allow to find the oldest or the newest member first.
- Case sensitive or unsensitive search.
- Show or skip multiple occurrences in one member.

Sources:
- Tool0009.cmd
- Tool0009.rpgle

### Tool0010

**Exit program for ODBC connect (to protocol or deny requests).**

Case: You would like to know (and collect into a table) who and how (ODBC, JDBC, OLE DB, .NET) is connection to your system.
This exit point allows you to allow or block requests.

Features:
- Collect information in the table QGPL.ODBCLOG.
- Allow or deny connect requests.

Sources:
- Tool0010.sqlrpgle

### Tool0011

**Searching for message definition**

Case: You have a dump file with a message id you haven't seen before. First you are trying to use the DSPMSGD command with the QCPFMSG message file to get more information but the message file QCPFMSG doesn't contain the message id you are looking for. The question is in which message file is this message id defined?

Features:
- Allow to search in all or only in defined message files.

Sources:
- Tool0011.cmd
- Tool0011.rpgle

## Coming soon...



