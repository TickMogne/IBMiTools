**Free

//
// Tool0009
//
// Created by: github.com/TickMogne, 2022.05.16
//
// Generate a list of physical file members containing a text.
//
// Compile parameters:
//  INCDIR('xxx/TMLib/src/') where xxx the parent library of the repository TMLib (github.com/TickMogne/TMLib)
//

Ctl-Opt DftActGrp(*No) Main(Tool0009) BndDir('TMLIB_M');

Dcl-F QSQPTABL Disk(2000) Usage(*Input) UsrOpn;
Dcl-F QSysPrt Printer(132) Usage(*Output) UsrOpn OflInd(QSysPrtOverflow);

/include TMApi_Inc.rpgle
/include TMLib_Inc.rpgle

Dcl-S QFile Char(20);
Dcl-S Text Char(32);
Dcl-S Output Char(6);
Dcl-S Order Char(11);
Dcl-S CaseSensitive Char(4);
Dcl-S MultipleOccurrences Char(5);

Dcl-Ds MemberEntryDS Len(100) Template Qualified;
  Name Char(10) Pos(1);
  DateTime Char(13) Pos(21);
End-Ds;

Dcl-Proc Tool0009;
  Dcl-Pi Tool0009;
    YYQFile Char(20);
    YYText Char(32);
    YYOutput Char(6);
    YYOrder Char(11);
    YYCaseSensitive Char(4);
    YYMultipleOccurrences Char(5);
  End-Pi;
  Dcl-S UserSpaceName Char(20) Inz('MEMBERLISTQTEMP');
  Dcl-S i Int(10);
  Dcl-S UserSpaceDataPointer Pointer;
  Dcl-Ds UserSpaceHeader based(UserSpaceDataPointer) Qualified;
    ListOffset Int(10) Pos(125);
    EntryCount Int(10) Pos(133);
    EntrySize Int(10) Pos(137);
  End-Ds;
  Dcl-S MemberEntryPointer Pointer;
  Dcl-Ds MemberEntry LikeDs(MemberEntryDS) Based(MemberEntryPointer);  
  Dcl-S QFileRet Char(20);
  Dcl-Ds FILD0200 Len(256) Qualified;
    RecordLength Int(10) Pos(67);
  End-Ds;
  Dcl-S Ret Int(10);
  Dcl-Ds Error LikeDs(ERRC0100);

  QFile = YYQFile;
  Text = YYText;
  Output = YYOutput;
  Order = YYOrder;
  CaseSensitive = YYCaseSensitive;
  MultipleOccurrences = YYMultipleOccurrences;

  // Create user space
  CreateUserSpace(UserSpaceName: Error);
  If (Error.BytesAvailable > 0); // Check error
    EscapeMessage(Error);
    Return;
  EndIf;

  // Get the file description
  qdbrtvfd(FILD0200: %size(FILD0200): QFileRet: 'FILD0200': QFile: '*FIRST': '0': '*LCL': '*INT': Error);
  If (Error.BytesAvailable > 0); // Check error
    EscapeMessage(Error);
    Return;
  EndIf;

  // Check if record longer than 2000
  If (FILD0200.RecordLength > 2000);
    EscapeMessage(CreateError('CPF9898': 'Files with recordlength over 2000 are not supported'));
    Return;
  EndIf;

  // Get the member list
  quslmbr(UserSpaceName: 'MBRL0200' :QFile: '*ALL': '0': Error);
  If (Error.BytesAvailable > 0); // Check error
    EscapeMessage(Error);
    Return;
  EndIf;

  // Get the user space pointer
  qusptrus(UserSpaceName: UserSpaceDataPointer: Error);
  If (Error.BytesAvailable > 0); // Check error
    EscapeMessage(Error);
    Return;
  EndIf;

  // Check if members were found
  If (UserSpaceHeader.EntryCount > 0);
    // Open the spool file - if needed
    If (Output = '*PRINT');
      QSysPrtOverflow = *On;
      Open QSysPrt;
    EndIf;

    // Get the pointer of the first member entry
    MemberEntryPointer = UserSpaceDataPointer + UserSpaceHeader.ListOffset;

    // Sort the members according creation date and time
    qsort(MemberEntryPointer: UserSpaceHeader.EntryCount: UserSpaceHeader.EntrySize: %Paddr(SortCompareProc));

    // Convert the text to uppercase if the search is no case sensitive
    Text = %Trim(Text) + x'00';
    If (CaseSensitive = '*NO');
      Text = Upper(Text);
    EndIf;

    // Process all members
    i = 1;
    Dow (i <= UserSpaceHeader.EntryCount);
      Ret = CheckMember(MemberEntry: FILD0200.RecordLength);
      // If output is display and F3 was pressed, exit
      If (Ret = 3);
        Leave;
      EndIf;
      // Shift the member entry pointer 
      MemberEntryPointer += UserSpaceHeader.EntrySize;
      i += 1;
    EndDo;

    // Close the spool file - if needed
    If (Output = '*PRINT');
      Close QSysPrt;  
    EndIf;
  EndIf;

  // Delete user space
  DeleteUserSpace(UserSpaceName);
End-Proc;

Dcl-Proc CheckMember;
  Dcl-Pi CheckMember Int(10);
    MemberEntry LikeDs(MemberEntryDS) Const;
    RecordLength Int(10) Const;
  End-Pi;
  Dcl-S LineCurrent Char(2000);
  Dcl-S LinePrevious Char(2000);
  Dcl-S LineNumber Int(10);
  Dcl-S Position Int(5);
  Dcl-Ds Line Len(2000) End-Ds;
  Dcl-S LineFull Char(4000);
  Dcl-S Ptr Pointer;
  Dcl-S Ret Int(10) Inz(0); // Not found
  Dcl-S Command Char(256);

  // Override to the member
  Command = 'OVRDBF FILE(QSQPTABL) TOFILE(' + %Trim(%Subst(QFile: 11: 10)) + '/' + %Trim(%Subst(QFile: 1: 10)) + ') MBR(' + %Trim(MemberEntry.Name)+ ')';
  qcmdexc(Command: %Len(%Trim(Command)));

  // Open the file
  Open QSQPTABL;

  // Read the file
  Read QSQPTABL Line;
  LineNumber += 1;
  // Read until not end of file
  Dow (%Eof(QSQPTABL) = *Off);
    LinePrevious = LineCurrent;
    LineCurrent = Line;
    // Convert the line to uppercase if the search is no case sensitive
    If (CaseSensitive = '*NO');
      LineCurrent = Upper(LineCurrent);
    EndIf;
    If (LineNumber > 1);
      // Build the two lines (previous+current) without the end of line
      Ptr = memcpy(%Addr(LineFull): %Addr(LinePrevious): RecordLength);
      Ptr = memcpy(%Addr(LineFull) + RecordLength: %Addr(LineCurrent): RecordLength);
      Ptr = memset(%Addr(LineFull) + 2 * RecordLength: 0: 1);
      // Search
      Ptr = strstr(%Addr(LineFull): %Addr(Text));
      // If the text was found
      If (Ptr <> *Null);
        Position = Ptr - %Addr(LineFull) + 1;
        If (Position <= RecordLength);
          Ret = 1; 
          If (Output = '*'); // Show
            Ret = Show(MemberEntry: LineNumber-1);
            // If F3 or F12 was pressed and skip multiple occurrences, exit from this member
            If ((Ret >= 2) Or (MultipleOccurrences = '*SKIP'));
              Leave;
            EndIf;
            If (Ret = 3);
              Leave;
            EndIf;
          ElseIf (Output = '*PRINT'); // Print
            Print(MemberEntry: LineNumber-1: LineFull: Position: RecordLength);
          EndIf;
        EndIf;
      EndIf;
    EndIf;
    // Read the next line
    Read QSQPTABL Line;
    LineNumber += 1;
  EndDo;

  // If still needed check the last line
  If ((Ret = 0) Or ((Ret = 1) And (MultipleOccurrences = '*SHOW')));
    LineCurrent = Line;
    // Search
    Ptr = strstr(%addr(LineCurrent): %addr(Text));
    // If the text was found
    If (Ptr <> *Null);
      Ret = 1;
      If (Output = '*'); // Show
        Ret = Show(MemberEntry: LineNumber-1);
      ElseIf (Output = '*PRINT'); // Print
        Print(MemberEntry: LineNumber: LineFull: Position: RecordLength);
      EndIf;
    EndIf;
  EndIf;

  // Close the file
  Close QSQPTABL;

  Return Ret;
End-Proc;

Dcl-Proc Show;
  Dcl-Pi Show Int(10);
    MemberEntry LikeDs(MemberEntryDs) Const;
    LineNumber Int(10) Const;
  End-Pi;
  Dcl-S Ret Int(10) Inz(1);
  Dcl-Ds JOBI0600 Len(332) Qualified;
    ExitKey Char(1) Pos(103);
    CancelKey Char(1) Pos(104);
  End-Ds;
  Dcl-Ds Error LikeDs(ERRC0100);
  Dcl-S Command Char(256);

  // Call DSPPFM and jump to the line
  Command = 'DSPPFM FILE(' + %Trim(%Subst(QFile:11:10)) + '/' + %Trim(%Subst(QFile:1:10)) + ') MBR(' + %Trim(MemberEntry.Name) + ') FROMRCD(' + %Trim(%Char(LineNumber)) + ')';
  qcmdexc(Command: %Len(%Trim(Command)));

  // Check if F3 or F12 was pressed
  qusrjobi(JOBI0600: %Size(JOBI0600): 'JOBI0600': '*': '': Error);
  If (JOBI0600.CancelKey = *On);
    Ret = 2; // Skip this member
  ElseIf (JOBI0600.ExitKey = *On);
    Ret = 3; // Skip all the rest of members
  EndIf;

  Return Ret;
End-Proc;

Dcl-Proc Print;
  Dcl-Pi Print;
    MemberEntry LikeDs(MemberEntryDs) Const;
    LineNumber Int(10) Const;
    Buffer Char(4000) Const;
    Position Int(5) Const;
    RecordLength Int(10) Const;
  End-Pi;
  Dcl-Ds QSysPrtHeader1 Len(132);
    *n Char(10) Pos(1) Inz('Library');
    *n Char(10) Pos(12) Inz('Name');
    *n Char(10) Pos(23) Inz('Member');
    *n Char(12) Pos(34) Inz('YYMMDDHHMMSS');
    *n Char(10) Pos(47) Inz('      Line');
    *n Char(5) Pos(58) Inz('  Pos');
    *n Char(64) Pos(64) Inz('Part of the line');
  End-Ds;
  Dcl-Ds QSysPrtHeader2 Len(132);
    *n Char(132) Pos(1) Inz('------------------------------------------------------------------------------------------------------------------------------------');
  End-Ds;
  Dcl-Ds QSysPrtLine Len(132) Qualified;
    Library Char(10) Pos(1);
    File Char(10) Pos(12);
    Member Char(10) Pos(23);
    DateTime Char(12) Pos(34);
    LineNumber Char(10) Pos(47);
    Position Char(5) Pos(58);
    Buffer Char(64) Pos(64);
  End-Ds;
  Dcl-S Start Int(10);
  Dcl-S Length Int(10);

  // Build the line
  QSysPrtLine.Library = %Subst(QFile: 11: 10);
  QSysPrtLine.File = %Subst(QFile: 1: 10);
  QSysPrtLine.Member = MemberEntry.Name;
  QSysPrtLine.DateTime = %Subst(MemberEntry.DateTime: 2: 12);
  QSysPrtLine.LineNumber = %EditW(LineNumber:'          ');
  QSysPrtLine.Position = %EditW(Position:'     ');
  Start = Position - 16;
  If (Start < 1);
    Start = 1;
  EndIf;
  Length = 64;
  If ((Start + Length) > (2 * RecordLength));
    Length = (2 * RecordLength) - Start + 1;
  EndIf; 
  QSysPrtLine.Buffer = %Subst(Buffer: Start: Length);
  // Print the header - if needed
  If QSysPrtOverflow = *On;
    Write QSysPrt QSysPrtHeader1;
    Write QSysPrt QSysPrtHeader2;
    QSysPrtOverflow = *Off;
  EndIf;
  // Print the line
  Write QSysPrt QSysPrtLine;
End-Proc;

Dcl-Proc SortCompareProc;
  Dcl-Pi SortCompareProc Int(10);
    Element1Ptr Pointer Value;
    Element2Ptr Pointer Value;
  End-Pi;
  Dcl-Ds Element1 LikeDs(MemberEntryDS) Based(Element1Ptr);
  Dcl-Ds Element2 LikeDs(MemberEntryDS) Based(Element2Ptr);

  If ((Order = '*DESCENDING') And (Element1.DateTime < Element2.DateTime));
    Return 1;
  ElseIf ((Order = '*DESCENDING') and (Element1.DateTime > Element2.DateTime));
    Return -1;
  ElseIf ((Order = '*ASCENDING') And (Element1.DateTime < Element2.DateTime));
    Return -1;
  ElseIf ((Order = '*ASCENDING') and (Element1.DateTime > Element2.DateTime));
    Return 1;
  Else;
    Return 0;
  EndIf;
End-Proc;

