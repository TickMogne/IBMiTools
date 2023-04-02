**Free

//
// Tool0017
//
// Created by: github.com/TickMogne, 2023.04.01
//
// List the journalled physical files in a journal.
//
// Compile parameters:
//  INCDIR('xxx/TMLib/src/') where xxx the parent library of the repository TMLib (github.com/TickMogne/TMLib)
//

Ctl-Opt DftActGrp(*No) Main(Tool0017) BndDir('TMLIB_M');

/include TMApi_Inc.rpgle
/include TMLib_Inc.rpgle

Dcl-Pr Tool0015 ExtPgm('TOOL0015');
  QualifiedDataAreaName Char(20);
End-Pr;
Dcl-Ds Tool0015DataArea Qualified;
  UserIndexName Char(20);
  NumberOfViews Int(10);
  NumberOfRecords Int(10);
  Title Char(78);
  Header1 Char(78);
  Header2 Char(78);
  // more Headers
End-Ds;
Dcl-Ds Tool0015Record Qualified;
  Key Int(10);
  Line1 Char(78);
  Line2 Char(78);
  // more Lines
End-Ds;

Dcl-Proc Tool0017;
  Dcl-Pi Tool0017;
    QJournalFile Char(20);
  End-Pi;
  
  Dcl-S DataAreaName Char(20);
  Dcl-Ds Error LikeDs(ERRC0100);
  Dcl-Ds RJRN0100 Qualified Based(RJRN0100_Pointer);
    BytesAvailable Int(10) Pos(5);
    OffsetToInformation Int(10) Pos(9);
  End-Ds;
  Dcl-S RJRN0100_Pointer Pointer;
  Dcl-S RJRN0100_Size Int(10);
  Dcl-Ds RJRN0100_Key_Information Len(20) Qualified;
    OffsetToInformation Int(10) Pos(5);
    LengthOfHeader Int(10) Pos(9);
    NumberOfEntry Int(10) Pos(13);
    LengthOfEntry Int(10) Pos(17);
  End-Ds;
  // Dcl-Ds RJRN0100_Key2_Header Len(36) Qualified;
  //   TotalFiles Int(10);
  // End-Ds;
  Dcl-Ds RJRN0100_Key2_Output Qualified Based(RJRN0100_Key2_Output_Pointer);
    ObjectType Char(10);
    ObjectName Char(10);
    ObjectLibrary Char(10);
    *N Char(16);
    FileType Char(1);
  End-Ds;
  Dcl-S RJRN0100_Key2_Output_Pointer Pointer;
  Dcl-DS InformationToRetrieve;
    *N Int(10) Inz(1); // Number of variable length records
      *N Int(10) Inz(22); // Length of variable length record
      *N Int(10) Inz(2); // Key
      *N Int(10) Inz(10); // Length of data
      *N Char(10) Inz('*FILE'); // Data
  End-Ds;
  Dcl-S I Int(10);
  Dcl-S Key Int(10);
  Dcl-S Ret Int(10);

  RJRN0100_Pointer = %Alloc(8);
  // Retrieve Journal Information - calculate only the buffer size needed
  QjoRetrieveJournalInformation(RJRN0100: -1: QJournalFile: 'RJRN0100': InformationToRetrieve:
    Error);
  If (Error.BytesAvailable > 0); // Check error
    EscapeMessage(Error);
    Return;
  EndIf;

  // Allocate buffer
  RJRN0100_Size = RJRN0100.BytesAvailable;
  Monitor;
    RJRN0100_Pointer = %Realloc(RJRN0100_Pointer: RJRN0100_Size);
  On-Error; // Check error
    memcpy(%Addr(Error.ExceptionData): %Addr(RJRN0100_Size): 4);
    EscapeMessage(CreateError('CPFAE14': Error.ExceptionData: 4));
    Return;
  EndMon;
  
  // Retrieve Journal Information
  QjoRetrieveJournalInformation(RJRN0100: RJRN0100_Size: QJournalFile: 'RJRN0100':
    InformationToRetrieve: Error);
  If (Error.BytesAvailable > 0); // Check error
    EscapeMessage(Error);
    Return;
  EndIf;

  memcpy(%Addr(RJRN0100_Key_Information): RJRN0100_Pointer + RJRN0100.OffsetToInformation + 4:
    %Size(RJRN0100_Key_Information));
//  memcpy(%Addr(RJRN0100_Key2_Header): RJRN0100_Pointer + RJRN0100.OffsetToInformation + 4 +
//    RJRN0100_Key_Information.OffsetToInformation: RJRN0100_Key_Information.LengthOfHeader);

  // Init the data area values
  Clear Tool0015DataArea;
  DataAreaName = CreateQualifiedTempFileName();
  Tool0015DataArea.UserIndexName = CreateQualifiedTempFileName();
  Tool0015DataArea.NumberOfViews = 1;
  Tool0015DataArea.Title = 'Journaled Physical Files in Journal ' +
    %Trim(%Subst(QJournalFile: 11: 10)) + '/' + %Trim(%Subst(QJournalFile: 1: 10));
  Tool0015DataArea.Header1 = 'Library     File';

  // Create the data area
  ExecuteCommand('CRTDTAARA DTAARA(' + %Trim(%Subst(DataAreaName: 11: 10)) + '/' +
    %Trim(%Subst(DataAreaName: 1: 10)) + ') TYPE(*CHAR) LEN(' +
    %Char(%Size(Tool0015DataArea) + Tool0015DataArea.NumberOfViews * %Size(Tool0015Record.Line1)) +
    ')': Error);
  If (Error.BytesAvailable > 0); // Check error
    EscapeMessage(Error);
    Return;
  EndIf;

  // Create the user index
  quscrtui(Tool0015DataArea.UserIndexName: '': 'F': %Size(Tool0015Record): '1':
    %Size(Tool0015Record.Key): '1': '0': '*EXCLUDE': '': '*YES': Error);
  If (Error.BytesAvailable > 0); // Check error
    EscapeMessage(Error);
    Return;
  EndIf;

  // Positioning to the first entry
  RJRN0100_Key2_Output_Pointer = RJRN0100_Pointer + RJRN0100.OffsetToInformation + 4 +
    RJRN0100_Key_Information.OffsetToInformation + RJRN0100_Key_Information.LengthOfHeader;

  // Loop through the entries
  I = 1;
  Tool0015Record.Key = 0;
  DoW (I <= RJRN0100_Key_Information.NumberOfEntry);
    If (RJRN0100_Key2_Output.FileType = '0');
      // Build the record
      Tool0015Record.Key += 1;
      %Subst(Tool0015Record.Line1: 1: 10) = RJRN0100_Key2_Output.ObjectLibrary;
      %Subst(Tool0015Record.Line1: 13: 10) = RJRN0100_Key2_Output.ObjectName;
      // Insert record
      qusaddui('': Ret: Tool0015DataArea.UserIndexName: 3: Tool0015Record: %Size(Tool0015Record):
        '': 1: Error);
      If (Error.BytesAvailable > 0); // Check error
        EscapeMessage(Error);
        Return;
      EndIf;
      Tool0015DataArea.NumberOfRecords += 1;
    EndIf;
    I += 1;
    // Positioning to the next entry
    RJRN0100_Key2_Output_Pointer += RJRN0100_Key_Information.LengthOfEntry;
  EndDo;

  // Free the buffer
  Dealloc RJRN0100_Pointer;

  // Write data area
  ChangeDataArea(DataAreaName: 1: %Size(Tool0015DataArea): Tool0015DataArea: Error);
  If (Error.BytesAvailable > 0); // Check error
    EscapeMessage(Error);
    Return;
  EndIf;

  Monitor;
   Tool0015(DataAreaName);
  On-Error;
    EscapeMessage(CreateError('CPFC406': '': 0));
  EndMon;

  // Delete the data area
  ExecuteCommand('DLTDTAARA DTAARA(' + %Trim(%Subst(DataAreaName: 11: 10)) + '/' +
    %Trim(%Subst(DataAreaName: 1: 10)) + ')': Error);
  If (Error.BytesAvailable > 0); // Check error
    EscapeMessage(Error);
    Return;
  EndIf;

  // Delete user index
  qusdltui(Tool0015DataArea.UserIndexName: Error);

End-Proc;