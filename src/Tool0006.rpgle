**Free

//
// Tool0006
//
// Created by: github.com/TickMogne, 2022.05.11
//
// Generate a list of diabled NetServer users.
//
// Compile parameters:
//  INCDIR('xxx/TMLib/src/') where xxx the parent library of the repository TMLib (github.com/TickMogne/TMLib)
//
// CPIB682 - User profile <TheUSRPRF> disabled for i5/OS Support for Windows Network Neighborhood access.
//

Ctl-Opt DftActGrp(*No) Main(Tool0006) BndDir('TMLIB_M');

Dcl-F QSysPrt Printer(132) Usage(*Output) OflInd(QSysPrtOverflow) UsrOpn;
Dcl-F QSQPTABL Disk(10) Usage(*Output) UsrOpn;

/include TMApi_Inc.rpgle
/include TMLib_Inc.rpgle

// Main procedure
Dcl-Proc Tool0006;
  Dcl-Pi Tool0006;
    Output Char(8) Const;
    OutputFile Char(20) Const;
  End-Pi;
  Dcl-S UserSpaceName Char(20) Inz('USERS     QTEMP');
  Dcl-Ds Error LikeDs(ERRC0100);
  Dcl-S UserSpaceDataPointer Pointer;
  Dcl-Ds UserSpaceHeader LikeDs(UserSpaceHeader_Ds) Based(UserSpaceDataPointer);
  Dcl-S i Int(10);
  Dcl-S UserSpaceEntryPointer Pointer;
  Dcl-Ds ZLSL0900 Based(UserSpaceEntryPointer) Qualified;
    UserName Char(10) Pos(1);
  End-Ds;
  Dcl-Ds QSysPrtLine Len(132) Qualified;
    UserName Char(10);
  End-Ds;
  Dcl-S MessageText Char(240);
  Dcl-Ds CSTK0100 Len(65535) Qualified;
    BytesReturned Int(10);
    BytesAvailable Int(10);
    NumberOfEntries Int(10);
    OffsetEntries Int(10);
    NumberOfEntriesReturned Int(10);
  End-Ds;
  Dcl-Ds  JIDF0100 Qualified;
    JobName Char(10) Inz('*');
    UserName Char(10) Inz(*Blanks);
    JobNumber Char(6) Inz(*Blanks);
    InternalJobId Char(16) Inz(*Blanks);
    *N Char(2) Inz(*Loval);
    ThreadIndicator Int(10) Inz(1);
    ThreadIdentifier Char(8) Inz(*Loval);
  End-Ds;
  Dcl-S CallStackEntryPointer Pointer;
  Dcl-Ds CallStackEntry Based(CallStackEntryPointer) Qualified;
    Length Int(10);
    DisplaceToStatements Int(10);
    NumberOfStatements Int(10);
    DisplaceToProcedureName Int(10);
    LengthOfProcedureName Int(10);
    RequestLevel Int(10);
    ProgramName Char(10) Pos(25);
    ProgramLibrary Char(10) Pos(35); 
  End-Ds;
  Dcl-S CallingProgramName Char(10);
  Dcl-S Command Char(256);
  Dcl-Ds QSQPTABLLine Len(10) Qualified;
    UserName Char(10);
  End-Ds;

  QSysPrtOverflow = *On;

  // Check which program is calling this program
  // to decide this program was called because of validation or not
  qwvrcstk(CSTK0100: %Size(CSTK0100): 'CSTK0100': JIDF0100: 'JIDF0100': Error);
  If (Error.BytesAvailable > 0); // Check error
    EscapeMessage(Error);
  EndIf;
  CallStackEntryPointer = %Addr(CSTK0100) + CSTK0100.OffsetEntries;
  For i = 1 To CSTK0100.NumberOfEntriesReturned;
    If (CallStackEntry.ProgramName <> ProgramStatus.Proc);
      CallingProgramName = CallStackEntry.ProgramName;
      Leave;
    EndIf;
    CallStackEntryPointer += CallStackEntry.Length;
  EndFor;

  // Command validation
  If (CallingProgramName = 'QCATRS');
    If ((Output = '*OUTFILE') And (OutputFile = *Blanks));
      MessageText = '0000' + GetMessageText('CPD9861': '': 0);
      qmhsndpm('CPD0006': 'QCPFMSG   *LIBL': MessageText: %Len(%Trim(MessageText)): '*DIAG': '*PGMBDY': 1: '': Error);
      EscapeMessage(CreateError('CPF0002'));
    EndIf;
    If ((Output = '*PRINT') And (OutputFile <> *Blanks));
      MessageText = '0000' + GetMessageText('CPD9862': '': 0);
      qmhsndpm('CPD0006': 'QCPFMSG   *LIBL': MessageText: %Len(%Trim(MessageText)): '*DIAG': '*PGMBDY': 1: '': Error);
      EscapeMessage(CreateError('CPF0002'));
    EndIf;
    Return;
  EndIf;

  // Not a command validation, process

  // Create user space
  CreateUserSpace(UserSpaceName: Error);
  If (Error.BytesAvailable > 0); // Check error
    EscapeMessage(Error);
    Return;
  EndIf;

  // Disabled user profiles
  qzlslsti(UserSpaceName: 'ZLSL0900': *Blanks: Error);
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

  // Check if disabled users were found
  If (UserSpaceHeader.NumberOfEntries > 0);
    If (Output = '*PRINT');
      // Open the printer file
      Open QSysPrt;
    Else;
      // Create and open the file
      Monitor;
        Command = 'CRTPF FILE(' + %Trim(%Subst(OutputFile: 11: 10)) + '/' + %Trim(%Subst(OutputFile: 1: 10)) + ') RCDLEN(10)';
        qcmdexc(%Trim(Command): %Len(%Trim(Command)));
        Command = 'OVRDBF FILE(QSQPTABL) TOFILE(' + %Trim(%Subst(OutputFile: 11: 10)) + '/' + %Trim(%Subst(OutputFile: 1: 10)) + ')';
        qcmdexc(%Trim(Command): %Len(%Trim(Command)));
      On-Error;
        EscapeMessage(CreateError('CPF0001': ProgramStatus.Proc: 10));
      EndMon;
      Open QSQPTABL;
    EndIf;

    // Init the entry pointer
    UserSpaceEntryPointer = UserSpaceDataPointer + UserSpaceHeader.OffsetListData;
    For i = 1 To UserSpaceHeader.NumberOfEntries;
      // Process all disabled users
    
      Select;
        When Output = '*PRINT';
          QSysPrtLine.UserName = ZLSL0900.UserName;
          Write QSysPrt QSysPrtLine;
        When Output = '*OUTFILE';
          QSQPTABLLine.UserName = ZLSL0900.UserName;
          Write QSQPTABL QSQPTABLLine;
      EndSl;

      // Shift the entry pointer
      UserSpaceEntryPointer += UserSpaceHeader.SizeOfEntry;
    EndFor;

    If (Output = '*PRINT');
      // Close the printer file
      Close QSysPrt;
    Else;
      Close QSQPTABL;
    EndIf;
  EndIf;

  // Delete user space
  DeleteUserSpace(UserSpaceName);

End-Proc;
