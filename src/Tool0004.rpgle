**Free

//
// Tool0004
//
// Created by: github.com/TickMogne, 2022.05.05
//
// Generate a list of a physical file dependencies with key field list information.
// See the command Tool0004.
//
// Compile parameters:
//  INCDIR('xxx/TMLib/src/') where xxx the parent library of the repository TMLib (github.com/TickMogne/TMLib)
//

Ctl-Opt DftActGrp(*No) Main(Tool0004) BndDir('TMLIB_M');

Dcl-F QSysPrt Printer(132) Usage(*Output) OflInd(QSysPrtOverflow) UsrOpn;

/include TMApi_Inc.rpgle
/include TMLib_Inc.rpgle

Dcl-Proc Tool0004;
  Dcl-Pi Tool0004;
    QFile Char(20);
  End-Pi;
  Dcl-S UserSpaceName Char(20) Inz('RELATIONS QTEMP');
  Dcl-DS Error LikeDS(ERRC0100);
  Dcl-S UserSpaceDataPointer Pointer;
  Dcl-Ds UserSpaceHeader LikeDs(UserSpaceHeader_Ds) Based(UserSpaceDataPointer);
  Dcl-S UserSpaceEntryPointer Pointer;
  Dcl-Ds DBRL0100 Len(320) Based(UserSpaceEntryPointer) Qualified;
    DepFileName Char(10) Pos(21);
    DepFileLibrary Char(10) Pos(31);
    DepType Char(1) Pos(41);
    JoinRefNo Int(10) Pos(45);
  End-Ds;
  Dcl-Ds OBJD0400 Len(675) Qualified;
    LastUsedDate Char(7) Pos(461);
  End-Ds;
  Dcl-S SelectOmit Char(1);
  Dcl-S i Int(10);
  Dcl-Ds FILD0100 Len(400) Qualified;
    //Qdb_Qdbfh;
    Attr Int(3) Pos(10);
  End-Ds;
  Dcl-Ds FILD0300 Len(65535) Qualified;
  End-Ds;
  Dcl-S Qdb_Qdbwh_Ptr Pointer;
  Dcl-Ds Qdb_Qdbwh Len(24) Qualified Based(Qdb_Qdbwh_Ptr);
    Fmt_Counts Int(5) Pos(23);
  End-Ds Qdb_Qdbwh;
  Dcl-S Qdb_Qdbwhrec_Ptr Pointer;
  Dcl-Ds Qdb_Qdbwhrec Len(32) Qualified Based(Qdb_Qdbwhrec_Ptr);
    Num_Of_Keys Int(5) Pos(13);
    Key_Info_Offset Int(10) Pos(29);
  End-Ds;
  Dcl-S Qdb_Qdbwhkey_Ptr Pointer;
  Dcl-Ds Qdb_Qdbwhkey Len(64) Qualified Based(Qdb_Qdbwhkey_Ptr);
    Name Char(10) Pos(1);
    Attr Int(3) Pos(29);
  End-Ds;
  Dcl-S ErrorText Char(240);
  Dcl-S KeyInfo Char(132);
  Dcl-S fo Int(10);
  Dcl-S fi Int(10);
  Dcl-S Descending Char(3);
  Dcl-Ds QSysPrtHeader1 Len(132);
    *n Char(10) Pos(1) Inz('Library');
    *n Char(10) Pos(12) Inz('File');
    *n Char(54) Pos(23) Inz('Information');
  End-Ds;
  Dcl-Ds QSysPrtHeader2 Len(132);
    *n Char(10) Pos(1) Inz('----------');
    *n Char(10) Pos(12) Inz('----------');
    *n Char(110) Pos(23) Inz('--------------------------------------------------------------------------------------------------------------');
  End-Ds;
  Dcl-Ds QSysPrtLine Len(132);
  End-Ds;

  // Create user space
  CreateUserSpace(UserSpaceName: Error);
  If (Error.BytesAvailable > 0); // Check error
    EscapeMessage(Error);
    Return;
  EndIf;

  // List database relations
  qdbldbr(UserSpaceName: 'DBRL0100': QFile: '*FIRST': '*ALL': Error);
  If (Error.BytesAvailable > 0); // Check error
    EscapeMessage(Error);
    Return;
  EndIf;

  // Retrieve the user space pointer
  qusptrus(UserSpaceName: UserSpaceDataPointer: Error);
  If (Error.BytesAvailable > 0); // Check error
    EscapeMessage(Error);
    Return;
  EndIf;

  // Open printer file
  Open QSysPrt;
  QSysPrtOverflow = *On;

  // Init the entry pointer
  UserSpaceEntryPointer = UserSpaceDataPointer + UserSpaceHeader.OffsetListData;

  // Sort the items (Library+Name)
  qsort(UserSpaceEntryPointer: UserSpaceHeader.NumberOfEntries: UserSpaceHeader.SizeOfEntry: %PAddr(SortCompareProc));

  // Check if libraries were found
  If UserSpaceHeader.NumberOfEntries > 0;
    // Process all dependent file
    For i = 1 To UserSpaceHeader.NumberOfEntries;
      // Get the last-used date
      qusrobjd(OBJD0400: %Size(OBJD0400): 'OBJD0400': DBRL0100.DepFileName + DBRL0100.DepFileLibrary: '*FILE': Error);
      If (Error.BytesAvailable > 0); // Check error
        Close QSysPrt;
        EscapeMessage(Error);
        Return;
      EndIf;

      // Check if Select/Omit option
      SelectOmit = '-';
      qdbrtvfd(FILD0100: %Size(FILD0100): '': 'FILD0100': DBRL0100.DepFileName + DBRL0100.DepFileLibrary: '*FIRST': '0': '*LCL': '*INT': Error);
      If (Error.BytesAvailable > 0); // Check error
        Close QSysPrt;
        EscapeMessage(Error);
        Return;
      EndIf;
      If (%BitAnd(FILD0100.Attr:64) = 64);
        SelectOmit = 'S';
      EndIf;

      // Get the index keys
      qdbrtvfd(FILD0300: %Size(FILD0300): '': 'FILD0300': DBRL0100.DepFileName + DBRL0100.DepFileLibrary: '*FIRST': '0': '*LCL': '*INT': Error);
      If ((Error.BytesAvailable > 0) And (Error.ExceptionId = 'CPF3270')); // Check error
        ErrorText = GetMessageText(Error.ExceptionId: Error.ExceptionData: Error.BytesAvailable-16);
        // Set the information tp print       
        QSysPrtLine = DBRL0100.DepFileLibrary + ' ' + DBRL0100.DepFileName + ' ' +
          %Subst(OBJD0400.LastUsedDate:2:6) + ' ' + DBRL0100.DepType + ' ' + %Char(DBRL0100.JoinRefNo) + ' ' + SelectOmit + ' ' + ErrorText;
      Else;
        If (Error.BytesAvailable > 0); // Check error
          Close QSysPrt;
          EscapeMessage(Error);
          Return;
        EndIf;
        // Set the header pointer
        Qdb_Qdbwh_Ptr = %addr(FILD0300);
        // Build the key information
        KeyInfo = '';
        fo = 1;
        Dow (fo <= Qdb_Qdbwh.Fmt_Counts);
          // Set the record information pointer
          Qdb_Qdbwhrec_Ptr = %Addr(FILD0300) + %Size(Qdb_Qdbwh) + (fo -1) * %Size(Qdb_Qdbwhrec);
          // Set the key information pointer
          Qdb_Qdbwhkey_Ptr = %Addr(FILD0300) + Qdb_Qdbwhrec.Key_Info_Offset;
          fi = 1;
          Dow (fi <= Qdb_Qdbwhrec.Num_Of_Keys);
            // Check if the order is descending
            Descending = '';
            If (%BitAnd(Qdb_Qdbwhkey.Attr:128) = 128);
              Descending = '(D)';
            EndIf;
            // Append to KeyInfo
            If (%Len(%Trim(KeyInfo)) <> 0);
              KeyInfo = %Trim(KeyInfo) + ', ' + %Trim(Qdb_Qdbwhkey.Name) + %Trim(Descending);
            Else;
              KeyInfo = %Trim(Qdb_Qdbwhkey.Name) + %Trim(Descending);
            EndIf;
            fi += 1;
            // Shift the key information pointer
            Qdb_Qdbwhkey_Ptr += %size(Qdb_Qdbwhkey);
          EndDo;
          fo += 1;
        EndDo;
        // Set the information tp print       
        QSysPrtLine = DBRL0100.DepFileLibrary + ' ' + DBRL0100.DepFileName + ' ' +
          %Subst(OBJD0400.LastUsedDate:2:6) + ' ' + DBRL0100.DepType + ' ' + %Char(DBRL0100.JoinRefNo) + ' ' + SelectOmit + ' ' + 
          %Trim(KeyInfo);
      EndIf;

      // Print the information
      If (QSysPrtOverflow = *On);
        Write QSysPrt QSysPrtHeader1;
        Write QSysPrt QSysPrtHeader2;
        QSysPrtOverflow = *Off;
      EndIf;
      Write QSysPrt QSysPrtLine;

      // Shift the entry pointer
      UserSpaceEntryPointer += UserSpaceHeader.SizeOfEntry;
    EndFor;
  EndIf;

  // Close printer file
  Close QSysPrt;

  // Delete user space
  DeleteUserSpace(UserSpaceName);

End-Proc;

// Compare function to sort
Dcl-Proc SortCompareProc;
  Dcl-Pi SortCompareProc Int(10);
    Element1Ptr Pointer Value;
    Element2Ptr Pointer Value;
  End-Pi;

  If (memcmp(Element1Ptr+30: Element2Ptr+30: 10) = 0); // Library1 = Library2
    Return memcmp(Element1Ptr+20: Element2Ptr+20: 10); // Name1 ? Name2
  Else;
    Return memcmp(Element1Ptr+30: Element2Ptr+30: 10); // Library1 ? Library2
  EndIf;
End-Proc;
