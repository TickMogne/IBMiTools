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

/include TMApi_Inc.rpgle
/include TMLib_Inc.rpgle

// Main procedure
Dcl-Proc Tool0006;
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

  QSysPrtOverflow = *On;

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
    // Open the printer file
    Open QSysPrt;
    // Init the entry pointer
    UserSpaceEntryPointer = UserSpaceDataPointer + UserSpaceHeader.OffsetListData;
    For i = 1 To UserSpaceHeader.NumberOfEntries;
      // Process all disabled users
      QSysPrtLine.UserName = ZLSL0900.UserName;
      Write QSysPrt QSysPrtLine;
      // Shift the entry pointer
      UserSpaceEntryPointer += UserSpaceHeader.SizeOfEntry;
    EndFor;
    // Close the printer file
    Close QSysPrt;
  EndIf;

  // Delete user space
  DeleteUserSpace(UserSpaceName);

End-Proc;
