**Free

//
// Tool0013
//
// Created by: github.com/TickMogne, 2022.11.16
//
// Update objects authorities.
//
// Compile parameters:
//  INCDIR('xxx/TMLib/src/') where xxx the parent library of the repository TMLib (github.com/TickMogne/TMLib)
//

Ctl-Opt DftActGrp(*No) Main(Tool0013) BndDir('TMLIB_M');

Dcl-F QSysPrt Printer(132) Usage(*Output) UsrOpn;

/include TMApi_Inc.rpgle
/include TMLib_Inc.rpgle

Dcl-Pr CheckAction Char(10);
  User Char(10) Const;
  Library Char(10) Const;
  Object Char(10) Const;
  Type Char(7) Const;
End-Pr; 

Dcl-Pr LibraryObjects;
  Library Char(10) Const;
  User Char(10) Const;
End-Pr;

Dcl-Ds DefinitionEntry Qualified;
  User Char(10) Pos(1);
  Library Char(10) Pos(11);
  Object Char(10) Pos(21);
  Type Char(7) Pos(31);
  Action Char(10) Pos(38);
End-Ds;

Dcl-Ds Definition LikeDs(DefinitionEntry) Dim(10);

// Main procedure
Dcl-Proc Tool0013;
  Dcl-Pi Tool0013;
   User Char(10);
  End-Pi;
  Dcl-Ds Error LikeDs(ERRC0100); 
  Dcl-S UserSpaceName Char(20) Inz('LIBRARIES QTEMP');
  Dcl-S UserSpaceDataPointer Pointer;
  Dcl-Ds UserSpaceHeader LikeDs(UserSpaceHeader_Ds) Based(UserSpaceDataPointer);
  Dcl-S UserSpaceEntryPointer Pointer;
  Dcl-Ds OBJL0100 Based(UserSpaceEntryPointer) Len(30) Qualified;
    Name Char(10) Pos(1);
  End-Ds;
  Dcl-S i Int(10);
  Dcl-S Action Char(10);
  Dcl-Ds QSysPrtLine Len(132) Qualified;
    Library Char(10) Pos(1);
    Object Char(10) Pos(12);
    Type Char(7) Pos(23);
    Action Char(10) Pos(31);
  End-Ds;

  Definition(1)  = 'QLIK      *ALL      *ALL      *ALL   *EXCLUDE  ';
  Definition(2)  = 'QLIK      Q*        *ALL      *ALL   *SKIP     ';
  Definition(3)  = 'QLIK      #*        *ALL      *ALL   *SKIP     ';
  Definition(4)  = 'QLIK      HA*       *ALL      *ALL   *SKIP     ';
  Definition(5)  = 'QLIK      SYS*      *ALL      *ALL   *SKIP     ';
  Definition(6)  = 'QLIK      DM*       *ALL      *ALL   *SKIP     ';
  Definition(7)  = 'QLIK      QLIK      *ALL      *ALL   *SKIP     ';
  Definition(8)  = 'QLIK      QGPL      *ALL      *ALL   *EXCLUDE  ';
  Definition(9)  = 'QLIK      QFTP*     *ALL      *ALL   *EXCLUDE  ';
  Definition(10) = 'QLIK      RHDBD_21  LIEF      *FILE  *USE      ';

  // Create user space
  CreateUserSpace(UserSpaceName: Error);
  If (Error.BytesAvailable > 0); // Check error
    EscapeMessage(Error);
    Return;
  EndIf;

  // List objects (type *MSGF)
  quslobj(UserSpaceName: 'OBJL0100': '*ALL      QSYS      ': '*LIB': Error);
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

  UserSpaceEntryPointer = UserSpaceDataPointer + UserSpaceHeader.OffsetListData;
  i = 0;
  Dow (i < UserSpaceHeader.NumberOfEntries);
    // Check if the library must be skipped
    Action = CheckAction(User: OBJL0100.Name: '*ALL': '*ALL');
    // Print
    QSysPrtLine.Library = OBJL0100.Name;
    QSysPrtLine.Object = '*ALL';
    QSysPrtLine.Type = '*ALL';
    QSysPrtLine.Action = Action;
    Write QSysPrt QSysPrtLine;
    // If the library should not be skipped, check the objects in the library
    If (Action <> '*SKIP');
      LibraryObjects(User: OBJL0100.Name);
    EndIf;
    // Shift the entry pointer
    UserSpaceEntryPointer += UserSpaceHeader.SizeOfEntry;
    i += 1;
  EndDo;

  // Close the printer file
  Close QSysPrt;

  // Delete user space
  DeleteUserSpace(UserSpaceName);

End-Proc;

// LibraryObjects
Dcl-Proc LibraryObjects;
  Dcl-Pi LibraryObjects;
    User Char(10) Const;
    Library Char(10) Const;
  End-Pi;
  Dcl-Ds Error LikeDs(ERRC0100); 
  Dcl-S UserSpaceName Char(20) Inz('OBJECTS   QTEMP');
  Dcl-S UserSpaceDataPointer Pointer;
  Dcl-Ds UserSpaceHeader LikeDs(UserSpaceHeader_Ds) Based(UserSpaceDataPointer);
  Dcl-S UserSpaceEntryPointer Pointer;
  Dcl-Ds OBJL0100 Based(UserSpaceEntryPointer) Qualified;
    Name Char(10) Pos(1);
    Library Char(10) Pos(11);
    Type Char(10) Pos(21);
  End-Ds;
  Dcl-S Action Char(10);
  Dcl-s i Int(10);
  Dcl-Ds QSysPrtLine Len(132) Qualified;
    Library Char(10) Pos(1);
    Object Char(10) Pos(12);
    Type Char(7) Pos(23);
    Action Char(10) Pos(31);
    Message Char(50) Pos (42);
  End-Ds;
  Dcl-Ds USRA0100 Qualified;
    *N Int(10);
    *N Int(10) Inz(18);
    ObjectAuthority Char(10);
  End-Ds;
  Dcl-S Command Char(256);

  // Create user space
  CreateUserSpace(UserSpaceName: Error);
  If (Error.BytesAvailable > 0); // Check error
    EscapeMessage(Error);
    Return;
  EndIf;

  // List objects
  quslobj(UserSpaceName: 'OBJL0100': '*ALL      ' + Library: '*ALL': Error);
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

  UserSpaceEntryPointer = UserSpaceDataPointer + UserSpaceHeader.OffsetListData;
  i = 0;
  Dow (i < UserSpaceHeader.NumberOfEntries);
    // Check the action
    Action = CheckAction(User: Library: OBJL0100.Name: %Subst(OBJL0100.Type: 1: 7));

    // Retrieve User Authority to Object
    qsyrusra(USRA0100: %Size(USRA0100): 'USRA0100': User: OBJL0100.Name + Library: OBJL0100.Type: Error);
    If (Error.BytesAvailable = 0);
      // Check if action should be done
      If (USRA0100.ObjectAuthority <> Action);
        QSysPrtLine.Message = 'Changed, old ' + USRA0100.ObjectAuthority;
        Command = 'GRTOBJAUT OBJ(' + %Trim(Library) + '/' + %Trim(OBJL0100.Name) + ') OBJTYPE(' + %Trim(OBJL0100.Type) + ') USER(' + %Trim(User) + ') AUT(' + %Trim(Action) + ')';
        Monitor;
          qcmdexc(Command: %Len(%Trim(Command)));
        On-Error;
          QSysPrtLine.Message = 'Not changed, old ' + USRA0100.ObjectAuthority;
        EndMon;
      Else;
        QSysPrtLine.Message = 'Old ' + USRA0100.ObjectAuthority;
      EndIf;
    Else;
      QSysPrtLine.Message = 'Error ' + Error.ExceptionId;
    EndIf;

    // Print
    QSysPrtLine.Library = OBJL0100.Library;
    QSysPrtLine.Object = OBJL0100.Name;
    QSysPrtLine.Type = OBJL0100.Type;
    QSysPrtLine.Action = Action;
    Write QSysPrt QSysPrtLine;

    // Shift the entry pointer
    UserSpaceEntryPointer += UserSpaceHeader.SizeOfEntry;
    i += 1;
  EndDo;

  // Delete user space
  DeleteUserSpace(UserSpaceName);

End-Proc;

// CheckAction
Dcl-Proc CheckAction;
  Dcl-Pi CheckAction Char(10);
    User Char(10) Const;
    Library Char(10) Const;
    Object Char(10) Const;
    Type Char(7) Const;
  End-Pi;
  Dcl-S e Int(10);
  Dcl-S i Int(10);
  Dcl-S j Int(10);
  Dcl-S l Char(10);
  Dcl-S Action Char(10) Inz(*Blanks);

  e = %Elem(Definition);

  // Check User, Library, Object, Type
  i = 1;
  Dow ((i <= e) And (Action = *Blanks));
    If ((Definition(i).User = User) And (Definition(i).Library = Library) And (Definition(i).Object = Object) And (Definition(i).Type = Type));
      Action = Definition(i).Action;
      Return Action;
    EndIf;
    i = i + 1;
  EndDo;

  // Check User, Library, Object, *ALL
  i = 1;
  Dow ((i <= e) And (Action = *Blanks));
    If ((Definition(i).User = User) And (Definition(i).Library = Library) And (Definition(i).Object = Object) And (Definition(i).Type = '*ALL'));
      Action = Definition(i).Action;
      Return Action;
    EndIf;
    i = i + 1;
  EndDo;

  // Check User, Library, *ALL, *ALL
  i = 1;
  Dow ((i <= e) And (Action = *Blanks));
    If ((Definition(i).User = User) And (Definition(i).Library = Library) And (Definition(i).Object = '*ALL') And (Definition(i).Type = '*ALL'));
      Action = Definition(i).Action;
      Return Action;
    EndIf;
    i = i + 1;
  EndDo;

  // Check User, Library*, *ALL, *ALL
  j = %Len(%Trim(Library));
  Dow (j > 1);
    l = %Subst(Library: 1: j-1) + '*';
    i = 1;
    Dow ((i <= e) And (Action = *Blanks));
      If ((Definition(i).User = User) And (Definition(i).Library = l) And (Definition(i).Object = '*ALL') And (Definition(i).Type = '*ALL'));
        Action = Definition(i).Action;
        Return Action;
      EndIf;
      i = i + 1;
    EndDo;
    j = j - 1;
  EndDo;

  // Check User, *ALL, *ALL, *ALL for the default action
  i = 1;
  Dow ((i <= e) And (Action = *Blanks));
    If ((Definition(i).User = User) And (Definition(i).Library = '*ALL') And (Definition(i).Object = '*ALL') And (Definition(i).Type = '*ALL'));
      Action = Definition(i).Action;
      Return Action;
    EndIf;
    i = i + 1;
  EndDo;

  Return Action;
End-Proc;
