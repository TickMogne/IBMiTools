**Free

//
// Tool0011
//
// Created by: github.com/TickMogne, 2022.06.01
//
// Searching for message definition.
//
// Compile parameters:
//  INCDIR('xxx/TMLib/src/') where xxx the parent library of the repository TMLib (github.com/TickMogne/TMLib)
//

Ctl-Opt DftActGrp(*No) Main(Tool0011) BndDir('TMLIB_M');

Dcl-F QSysPrt Printer(132) Usage(*Output) OflInd(QSysPrtOverflow) UsrOpn;

/include TMApi_Inc.rpgle
/include TMLib_Inc.rpgle

// Main procedure
Dcl-Proc Tool0011;
  Dcl-Pi Tool0011;
    MessageId Char(7);
    MessageFile Char(20);
  End-Pi;
  Dcl-Ds Error LikeDs(ERRC0100);  
  Dcl-S UserSpaceName Char(20) Inz('MSGFILES  QTEMP');
  Dcl-S UserSpaceDataPointer Pointer;
  Dcl-Ds UserSpaceHeader LikeDs(UserSpaceHeader_Ds) Based(UserSpaceDataPointer);
  Dcl-S UserSpaceEntryPointer Pointer;
  Dcl-Ds OBJL0100 Based(UserSpaceEntryPointer) Qualified;
    Name Char(10) Pos(1);
    Library Char(10) Pos(11);
  End-Ds;
  Dcl-Ds RTVM0100 Len(8000) Qualified;
    MessageLength Int(10) Pos(9);
    Message Char(1) Pos(25);
  End-Ds;
  Dcl-S i Int(10);
  Dcl-Ds QSysPrtLine Len(132) Qualified;
    Library Char(10) Pos(1);
    File Char(10) Pos(12);
    MessageId Char(7) Pos(23);
    Message Char(100) Pos(31);
  End-Ds;
  Dcl-S l Int(10);
  Dcl-Ds QSysPrtHeader1 Len(132) End-Ds;
  Dcl-Ds QSysPrtHeader2 Len(132) End-Ds;
  Dcl-S f Int(10) Inz(0);

  QSysPrtOverflow = *On;
  QSysPrtHeader1 = 'Library    File       MsgId   Message';
  memset(%Addr(QSysPrtHeader2): 96: 132);

  // Create user space
  CreateUserSpace(UserSpaceName: Error);
  If (Error.BytesAvailable > 0); // Check error
    EscapeMessage(Error);
    Return;
  EndIf;

  // List objects (type *MSGF)
  quslobj(UserSpaceName: 'OBJL0100': MessageFile: '*MSGF': Error);
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

  // Open the printer file
  Open QSysPrt;

  i = 0;
  // Init the entry pointer
  UserSpaceEntryPointer = UserSpaceDataPointer + UserSpaceHeader.OffsetListData;
  Dow (i < UserSpaceHeader.NumberOfEntries);
    // Process the message file
    qmhrtvm(RTVM0100: %Size(RTVM0100): 'RTVM0100': MessageId: OBJL0100.Name + OBJL0100.Library: '': 0: '*NO': '*NO': Error);
    // Check if message id was found in the message file
    If (Error.BytesAvailable = 0);
      f += 1;
      // Build the line
      QSysPrtLine.Library = OBJL0100.Library;
      QSysPrtLine.File = OBJL0100.Name;
      QSysPrtLine.MessageId = MessageId;
      l = RTVM0100.MessageLength;
      If (l > 100);
        l = 100;
      EndIf;
      QSysPrtLine.Message = *Blanks;
      memcpy(%Addr(QSysPrtLine.Message): %Addr(RTVM0100.Message): l);
      // Print the line
      If (QSysPrtOverflow = *On);
        Write QSysPrt QSysPrtHeader1;
        Write QSysPrt QSysPrtHeader2;
        QSysPrtOverflow = *Off;
      EndIf;
      Write QSysPrt QSysPrtLine;
    EndIf;
    // Shift the entry pointer
    UserSpaceEntryPointer += UserSpaceHeader.SizeOfEntry;
    i += 1;
  EndDo;

  // Close the printer file
  Close QSysPrt;  

  // Delete user space
  DeleteUserSpace(UserSpaceName);

  // Check if entry was found
  If (f = 0);
    Error = CreateError('CPF2419': MessageId + MessageFile: 27);
    EscapeMessage(Error); 
  Else;
    InfoMessage(*Omit: %Char(f) + ' entries were found.');
  EndIf;

End-Proc;
