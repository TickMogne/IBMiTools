**Free

//
// Tool0016
//
// Created by: github.com/TickMogne, 2023.03.12
//
// List jobs they are locking a physical or logical file.
//
// Compile parameters:
//  INCDIR('xxx/TMLib/src/') where xxx the parent library of the repository TMLib
//  (github.com/TickMogne/TMLib)
//

Ctl-Opt DftActGrp(*No) Main(Tool0016) BndDir('TMLIB_M');

/include TMApi_Inc.rpgle
/include TMLib_Inc.rpgle

Dcl-Pr Tool0015 ExtPgm('TOOL0015');
  QualifiedDataAreaName Char(20);
End-Pr;
Dcl-Ds Tool0015DataArea Qualified;
  UserIndexName Char(20);
  NumberOfViews Int(10);
  NumberOfRecords Int(10);
  ReturnKey Char(1);
  Refresh Ind;  
  Title Char(78);
  Header1 Char(78);
  // more Headers
End-Ds;
Dcl-Ds Tool0015Record Qualified;
  Key Int(10);
  Line1 Char(78);
  Line2 Char(78);
  // more Lines
End-Ds;
Dcl-S UserIndexName Char(20);
Dcl-S Quit Ind;

// Main procedure
Dcl-Proc Tool0016;
  Dcl-Pi Tool0016;
    QFile Char(20);
  End-Pi;
  Dcl-Ds Error LikeDs(ERRC0100); 
  Dcl-S UserSpaceName1 Char(20);
  Dcl-S UserSpaceName2 Char(20);
  Dcl-S UserSpaceDataPointer Pointer;
  Dcl-Ds UserSpaceHeader LikeDs(UserSpaceHeader_Ds) Based(UserSpaceDataPointer);
  Dcl-S UserSpaceEntryPointer Pointer;
  Dcl-Ds DBRL0100 Len(320) Based(UserSpaceEntryPointer) Qualified;
    DepQFileName Char(20) Pos(21);
  End-Ds;
  Dcl-S i Int(10);

  Clear Tool0015DataArea;
  Tool0015DataArea.NumberOfViews = 1;
  Tool0015DataArea.ReturnKey = x'00';
  Tool0015DataArea.Title = 'List of jobs they are locking the file ' +
    %Trim(%Subst(QFile: 11: 10)) + '/' + %Trim(%Subst(QFile: 1: 10));
  Tool0015DataArea.Header1 = 'Job user    Job name    Job number';  
  Tool0015DataArea.Refresh = *On;

  Quit = *Off;

  DoW (Not Quit);

    Tool0015DataArea.NumberOfRecords = 0;

    // Create user index
    UserIndexName = CreateQualifiedTempFileName();
    quscrtui(UserIndexName: 'PF': 'F': 26: '1': 26: '1': '0': '*EXCLUDE': '': '*YES':
      Error);
    If (Error.BytesAvailable > 0); // Check error
      EscapeMessage(Error);
      Return;
    EndIf;

    // Create user space 1
    UserSpaceName1 = CreateQualifiedTempFileName();
    CreateUserSpace(UserSpaceName1: Error);
    If (Error.BytesAvailable > 0); // Check error
      EscapeMessage(Error);
      Return;
    EndIf;

    // List database relations
    qdbldbr(UserSpaceName1: 'DBRL0100': QFile: '*FIRST': '*ALL': Error);
    If (Error.BytesAvailable > 0); // Check error
      EscapeMessage(Error);
      Return;
    EndIf;

    // Retrieve the user space pointer
    qusptrus(UserSpaceName1: UserSpaceDataPointer: Error);
    If (Error.BytesAvailable > 0); // Check error
      EscapeMessage(Error);
      Return;
    EndIf;

    // Init the entry pointer
    UserSpaceEntryPointer = UserSpaceDataPointer + UserSpaceHeader.OffsetListData;

    // Create user space 2
    UserSpaceName2 = CreateQualifiedTempFileName();
    CreateUserSpace(UserSpaceName2: Error);
    If (Error.BytesAvailable > 0); // Check error
      EscapeMessage(Error);
      Return;
    EndIf;

    // Check the file itself
    CheckLock(UserSpaceName2: QFile: Error);
    If (Error.BytesAvailable > 0); // Check error
      EscapeMessage(Error);
      Return;
    EndIf; 

    // Check if dependencies were found
    If UserSpaceHeader.NumberOfEntries > 0;
      // Process all dependent file
      For i = 1 To UserSpaceHeader.NumberOfEntries;

        If DBRL0100.DepQFileName <> '*NONE';
          CheckLock(UserSpaceName2: DBRL0100.DepQFileName: Error);
          If (Error.BytesAvailable > 0); // Check error
            EscapeMessage(Error);
            Return;
          EndIf;
        EndIf;

        // Shift the entry pointer
        UserSpaceEntryPointer += UserSpaceHeader.SizeOfEntry;
      EndFor;
    EndIf;

    // Delete user space 2
    DeleteUserSpace(UserSpaceName2);

    // Delete user space 1
    DeleteUserSpace(UserSpaceName1);

    // Show the result with the Tool0015
    ShowResult();

    // Delete user index
    qusdltui(UserIndexName: Error);

  EndDo;
  
End-Proc;

Dcl-Proc CheckLock;
  Dcl-Pi CheckLock;
    UserSpaceName Char(20);
    QFile Char(20);
    Error LikeDs(ERRC0100);
  End-Pi;
  Dcl-S i Int(10);
  Dcl-S UserSpaceDataPointer Pointer;
  Dcl-Ds UserSpaceHeader LikeDs(UserSpaceHeader_Ds) Based(UserSpaceDataPointer);
  Dcl-S UserSpaceEntryPointer Pointer;
  Dcl-Ds OBJL0100 Len(64) Based(UserSpaceEntryPointer) Qualified;
    Job Char(26) Pos(1);
  End-Ds;
  Dcl-S IndexKey Char(26);
  Dcl-S IndexEntry Char(34);
  Dcl-S Elo Char(8);
  Dcl-S Ret Int(10);

  // List object locks
  qwclobjl(UserSpaceName: 'OBJL0100': QFile: '*FILE': '*ALL': Error);
  If (Error.BytesAvailable > 0); // Check error
    Return;
  EndIf;

  // Retrieve the user space pointer
  qusptrus(UserSpaceName: UserSpaceDataPointer: Error);
  If (Error.BytesAvailable > 0); // Check error
    Return;
  EndIf;

  // Init the entry pointer
  UserSpaceEntryPointer = UserSpaceDataPointer + UserSpaceHeader.OffsetListData;

  // Check if dependencies were found
  If UserSpaceHeader.NumberOfEntries > 0;
    // Process all dependent file
    For i = 1 To UserSpaceHeader.NumberOfEntries;
      // Build the index key of the job
      IndexKey = %Subst(OBJL0100.Job: 11: 10) + %Subst(OBJL0100.Job: 1: 10) +
        %Subst(OBJL0100.Job: 21: 6);
      If IndexKey <> *Blanks;
        // Check if the job already listed
        qusrtvui(IndexEntry: %Size(IndexEntry): Elo: 8: Ret: '': UserIndexName: 'IDXE0100': 1: 1:
          IndexKey: 26: 0: Error);
        If (Error.BytesAvailable > 0); // Check error
          EscapeMessage(Error);
          Return;
        EndIf;
        // If the job is not listed yet, append into the list
        If Ret = 0;
          qusaddui('': Ret: UserIndexName: 3: IndexKey: 26: '': 1: Error);
          If (Error.BytesAvailable > 0); // Check error
            EscapeMessage(Error);
            Return;
          EndIf;
          Tool0015DataArea.NumberOfRecords = Tool0015DataArea.NumberOfRecords + 1;
        EndIf;
      EndIf;

      // Shift the entry pointer
      UserSpaceEntryPointer += UserSpaceHeader.SizeOfEntry;
    EndFor;
  EndIf;
End-Proc;

Dcl-Proc ShowResult;
  Dcl-Ds IndexEntry Qualified;
    BytesReturned Int(10);
    BytesAvailable Int(10);
    Entry Char(26);
  End-Ds;
  Dcl-Ds Elo Qualified;
    BytesReturned Int(10);
    BytesAvailable Int(10);
  End-Ds;
  Dcl-S Ret Int(10);
  Dcl-Ds Error LikeDs(ERRC0100);
  Dcl-Ds ResultIndexEntry Qualified;
    KeyChar Char(4) Pos(1);
    KeyNum Int(10) Pos(1);
    Line1 Char(78) Pos(5);
    Line2 Char(78) Pos(83);
    Line3 Char(78) Pos(161);
  End-Ds;
  Dcl-S Command Char(256);
  Dcl-S DataAreaFileName Char(20);

  Tool0015DataArea.UserIndexName = CreateQualifiedTempFileName();

  // Create user index
  quscrtui(Tool0015DataArea.UserIndexName: 'PF': 'F': %Size(ResultIndexEntry): '1':
    %Size(ResultIndexEntry.KeyChar): '1': '0': '*EXCLUDE': '': '*YES': Error);
  If (Error.BytesAvailable > 0); // Check error
    EscapeMessage(Error);
    Return;
  EndIf;

  Clear ResultIndexEntry;

  // First entry
  qusrtvui(IndexEntry: %Size(IndexEntry): Elo: 8: Ret: '': UserIndexName: 'IDXE0100': 1: 6: '': 0:
    0: Error);
  If (Error.BytesAvailable > 0); // Check error
    EscapeMessage(Error);
    Return;
  EndIf;

  // Loop through the entries
  DoW (Ret = 1);
    ResultIndexEntry.KeyNum = ResultIndexEntry.KeyNum + 1;
    ResultIndexEntry.Line1 = %Subst(IndexEntry.Entry: 1: 10) + '  ' +
      %Subst(IndexEntry.Entry: 11: 10) + '  ' + %Subst(IndexEntry.Entry: 21: 6);
    ResultIndexEntry.Line2 = '';
    ResultIndexEntry.Line3 = '';
    qusaddui('': Ret: Tool0015DataArea.UserIndexName: 3: ResultIndexEntry: %Size(ResultIndexEntry):
      '': 1: Error);
    If (Error.BytesAvailable > 0); // Check error
      EscapeMessage(Error);
      Return;
    EndIf;
    
    qusrtvui(IndexEntry: %Size(IndexEntry): Elo: 8: Ret: '': UserIndexName: 'IDXE0100': 1: 2:
      IndexEntry.Entry: 26: 0: Error);
    If (Error.BytesAvailable > 0); // Check error
      EscapeMessage(Error);
      Return;
    EndIf;
  EndDo;

  // Create the data area
  DataAreaFileName = CreateQualifiedTempFileName();
  ExecuteCommand('CRTDTAARA DTAARA(' + %Trim(%Subst(DataAreaFileName: 11: 10)) + '/' +
    %Trim(%Subst(DataAreaFileName: 1: 10)) + ') TYPE(*CHAR) LEN(2000)': Error);
  If (Error.BytesAvailable > 0); // Check error
    EscapeMessage(Error);
    Return;
  EndIf;

  // Set the values in the data area
  ChangeDataArea(DataAreaFileName: 1: %Size(Tool0015DataArea): Tool0015DataArea: Error);
  If (Error.BytesAvailable > 0); // Check error
    EscapeMessage(Error);
    Return;
  EndIf;

  // Display the result
  Tool0015(DataAreaFileName);

  Tool0015DataArea = RetrieveDataArea(DataAreaFileName: 1: %Size(Tool0015DataArea): Error);
  If (Error.BytesAvailable > 0); // Check error
    EscapeMessage(Error);
    Return;
  EndIf;
  If (Tool0015DataArea.ReturnKey = x'03');
    Quit = *On;
  EndIf;

  // Delete the data area
  ExecuteCommand('DLTDTAARA DTAARA(' + %Trim(%Subst(DataAreaFileName: 11: 10)) + '/' +
    %Trim(%Subst(DataAreaFileName: 1: 10)) + ')': Error);
  If (Error.BytesAvailable > 0); // Check error
    EscapeMessage(Error);
    Return;
  EndIf;

  // Delete user index
  qusdltui(Tool0015DataArea.UserIndexName: Error);

End-Proc;
