**Free

//
// Tool0002
//
// Created by: github.com/TickMogne, 2022.05.01
//
// Generate a list of query manager queries containing a text.
// See the command Tool0002.
//
// Compile parameters:
//  INCDIR('xxx/TMLib/src/') where xxx the parent library of the repository TMLib (github.com/TickMogne/TMLib)
//

Ctl-Opt DftActGrp(*No) Main(Tool0002) BndDir('TMLIB_M');

Dcl-F QSysPrt Printer(132) Usage(*Output) OflInd(QSysPrtOverflow) UsrOpn;

/include TMApi_Inc.rpgle
/include TMLib_Inc.rpgle

Dcl-S Library Char(10);
Dcl-S Text Char(32);
Dcl-S SourceFileCreated Ind Inz(*Off);

// Main procedure
Dcl-Proc Tool0002;
  Dcl-Pi Tool0002;
    P_Library Char(10);                  
    P_Text Char(32);                  
  End-Pi;

  Dcl-S Command Char(128);

  Library = Upper(P_Library);
  Text = Upper(P_Text);

  QSysPrtOverflow = *On;

  // Open the printer file
  Open QSysPrt;

  // Check the qm queries
  Check();

  // Close the printer file
  Close QSysPrt;

  // Delete the temp file if created
  If (SourceFileCreated = *On);
    Command = 'DLTF FILE(QTEMP/QMQSRC)';
    qcmdexc(Command: %Len(%Trim(Command)));
  EndIf;
End-Proc;

// Check the Query Manager Queries
Dcl-Proc Check;
  Dcl-S UserSpaceName Char(20) Inz('LIBRARIES QTEMP');
  Dcl-Ds Error LikeDs(ERRC0100);
  Dcl-S i Int(10);
  Dcl-S UserSpaceDataPointer Pointer;
  Dcl-Ds UserSpaceHeader LikeDs(UserSpaceHeader_Ds) Based(UserSpaceDataPointer);
  Dcl-S UserSpaceEntryPointer Pointer;
  Dcl-Ds OBJL0100 based(UserSpaceEntryPointer) qualified;
    Name Char(10) Pos(1);
  End-Ds;

  If (%Scan('*': Library) = 0);
    CheckInLibrary(Library);
  Else;
    // Create User Space
    CreateUserSpace(UserSpaceName: Error);
    If (Error.BytesAvailable > 0); // Check error
      Close QSysPrt;
      EscapeMessage(Error);
      Return;
    EndIf;    

    // List objects (type *LIB in QSYS)
    quslobj(UserSpaceName:'OBJL0100':Library+'QSYS':'*LIB':Error);
    If (Error.BytesAvailable > 0); // Check error
      Close QSysPrt;
      EscapeMessage(Error);
      Return;
    EndIf;

    // Retrieve the user space pointer
    qusptrus(UserSpaceName:UserSpaceDataPointer:Error);
    If (Error.BytesAvailable > 0); // Check error
      Close QSysPrt;
      EscapeMessage(Error);
      Return;
    EndIf;

    // Check if libraries were found
    If UserSpaceHeader.NumberOfEntries > 0;
      // Init the entry pointer
      UserSpaceEntryPointer = UserSpaceDataPointer + UserSpaceHeader.OffsetListData;
      // Process all libraries
      For i = 1 To UserSpaceHeader.NumberOfEntries;
        CheckInLibrary(OBJL0100.Name);
        // Shift the entry pointer
        UserSpaceEntryPointer += UserSpaceHeader.SizeOfEntry;
      EndFor;
    EndIf;

    // Delete user space
    DeleteUserSpace(UserSpaceName: Error);

  EndIf;
End-Proc;

// Check the Query Manager Queries in the specified library
Dcl-Proc CheckInLibrary;
  Dcl-Pi CheckInLibrary;
    Library Char(10);
  End-Pi;
  Dcl-Ds QSysPrtHeader1 Len(132);
    *n Char(10) Pos(1) Inz('Library');
    *n Char(10) Pos(12) Inz('QMQ Name');
    *n Char(54) Pos(23) Inz('Line of the source');
  End-Ds;
  Dcl-Ds QSysPrtHeader2 Len(132);
    *n Char(10) Pos(1) Inz('----------');
    *n Char(10) Pos(12) Inz('----------');
    *n Char(80) Pos(23) Inz('--------------------------------------------------------------------------------');
  End-Ds;
  Dcl-Ds QSysPrtLine Len(132);
  End-Ds;
  Dcl-S UserSpaceName Char(20) Inz('OBJECTS   QTEMP');
  Dcl-Ds Error LikeDs(ERRC0100);
  Dcl-S i Int(10);
  Dcl-S UserSpaceDataPointer Pointer;
  Dcl-Ds UserSpaceHeader LikeDs(UserSpaceHeader_Ds) Based(UserSpaceDataPointer);
  Dcl-S UserSpaceEntryPointer Pointer;
  Dcl-Ds OBJL0100 Based(UserSpaceEntryPointer) Qualified;
    Name Char(10) Pos(1);
  End-Ds;
  Dcl-S LikeText Char(34);
  Dcl-S Data Char(80);
  Dcl-S Command Char(128);

  // Create user space
  CreateUserSpace(UserSpaceName: Error);
  If (Error.BytesAvailable > 0); // Check error
    Close QSysPrt;
    EscapeMessage(Error);
    Return;
  EndIf;  

  // List objects (type *QMQRY)
  quslobj(UserSpaceName:'OBJL0100':'*ALL      '+Library:'*QMQRY':Error);
  If (Error.BytesAvailable > 0); // Check error
    Close QSysPrt;
    EscapeMessage(Error);
    Return;
  EndIf;

  // Retrieve the user space pointer
  qusptrus(UserSpaceName:UserSpaceDataPointer:Error);
  If (Error.BytesAvailable > 0); // Check error
    Close QSysPrt;
    EscapeMessage(Error);
    Return;
  EndIf;

  // Check if libraries were found
  If (UserSpaceHeader.NumberOfEntries > 0);
    // Create the temp source file - if not yet created
    If (SourceFileCreated = *Off);
      Command = 'CRTSRCPF FILE(QTEMP/QMQSRC)';
      qcmdexc(Command:%Len(%Trim(Command)));
      SourceFileCreated = *On;
    EndIf;

    // Process all objects
    For i = 1 To UserSpaceHeader.NumberOfEntries;
      UserSpaceEntryPointer = UserSpaceDataPointer + UserSpaceHeader.OffsetListData + (i-1) * UserSpaceHeader.SizeOfEntry;

      // Retrieve QMQRY source
      Command = 'RTVQMQRY QMQRY(' + %Trim(Library) + '/' + %Trim(OBJL0100.Name) + ') SRCFILE(QTEMP/QMQSRC) SRCMBR(QMQ)';
      qcmdexc(Command:%Len(%Trim(Command)));

      //
      LikeText = '%' + %Trim(Text) + '%';
      Exec Sql SELECT srcdta AS data
        INTO :Data
        FROM qtemp/qmqsrc
        WHERE srcseq > 2 AND upper(srcdta) LIKE :LikeText // srcseq > 3 ?
        FETCH FIRST 1 ROWS ONLY;

      // If found
      If (SQLSTATE = '00000');
        If (QSysPrtOverflow = *On);
          Write QSysPrt QSysPrtHeader1;
          Write QSysPrt QSysPrtHeader2;
          QSysPrtOverflow = *Off;
        EndIf;
        QSysPrtLine = Library + ' ' + OBJL0100.Name + ' ' + Data;
        Write QSysPrt QSysPrtLine;
      EndIf;

    EndFor;

  EndIf;

  // Delete user space
  DeleteUserSpace(UserSpaceName);
End-Proc;

