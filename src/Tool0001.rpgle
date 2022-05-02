**Free

//
// Tool0001
//
// Created by: github.com/TickMogne, 2022.05.01
//
// Generate a list of job descriptions whose library list contains one or all of the given libraries.
// See the command Tool0001.
//
// Compile parameters:
//  INCDIR('xxx/TMLib/src/') where xxx the parent library of the repository TMLib (github.com/TickMogne/TMLib)
//

Ctl-Opt DftActGrp(*No) Main(Tool0001) BndDir('TMLIB_M');

Dcl-F QSysPrt Printer(132) Usage(*Output) OflInd(QSysPrtOverflow) UsrOpn;

/include TMApi_Inc.rpgle
/include TMLib_Inc.rpgle

Dcl-S Library Char(10);
Dcl-S Libraries Char(10) Dim(5) Inz(*Blanks);
Dcl-S Match Char(4);

// Main procedure
Dcl-Proc Tool0001;
  Dcl-Pi Tool0001;
    P_Lib Char(10);
    P_Libs LikeDs(P_Libs_Ds);
    P_Match Char(4);                        
  End-Pi;

  Dcl-Ds P_Libs_Ds Qualified Template;                   
    Count Int(5);
    Libraries Char(10) Dim(5);                  
  End-Ds;
  Dcl-S i Int(5);

  // Process the entry parameters
  Library = Upper(P_Lib);
  For i = 1 To P_Libs.Count;
    Libraries(i) = Upper(P_Libs.Libraries(i));
  EndFor;
  Match = P_Match;

  QSysPrtOverflow = *On;

  // Open the printer file
  Open QSysPrt;

  // Check the job descriptions
  Check();

  // Close the printer file
  Close QSysPrt;
End-Proc;

// Check the job descriptions
Dcl-Proc Check;
  Dcl-S UserSpaceName Char(20) Inz('LIBRARIES QTEMP');
  Dcl-Ds Error LikeDs(ERRC0100);
  Dcl-S i Int(10);
  Dcl-S UserSpaceDataPointer Pointer;
  Dcl-Ds UserSpaceHeader LikeDs(UserSpaceHeader_Ds) Based(UserSpaceDataPointer);
  Dcl-S UserSpaceEntryPointer Pointer;
  Dcl-Ds OBJL0100 Based(UserSpaceEntryPointer) Qualified;
    Name Char(10) Pos(1);
  End-Ds;

  // Check if specified library should be check
  If (%Scan('*': Library) = 0);
    CheckInLibrary(Library);
  Else;
    // Create user space
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

// Check the job descriptions in the library
Dcl-Proc CheckInLibrary;
  Dcl-Pi CheckInLibrary;
    Library Char(10);
  End-Pi;

  Dcl-Ds QSysPrtHeader1 Len(132);
    *n Char(10) Pos(1) Inz('Library');
    *n Char(10) Pos(12) Inz('Name');
    *n Char(54) Pos(23) Inz('Libraries found');
  End-Ds;
  Dcl-Ds QSysPrtHeader2 Len(132);
    *n Char(10) Pos(1) Inz('----------');
    *n Char(10) Pos(12) Inz('----------');
    *n Char(54) Pos(23) Inz('------------------------------------------------------');
  End-Ds;
  Dcl-Ds QSysPrtLine Len(132) Qualified;
    Libs Char(11) Pos(23) Dim(5);
  End-Ds;
  Dcl-S UserSpaceName Char(20) Inz('OBJECTS   QTEMP');
  Dcl-Ds Error LikeDs(ERRC0100);
  Dcl-S i Int(10);
  Dcl-S j Int(10);
  Dcl-S k Int(10);
  Dcl-S UserSpaceDataPointer Pointer;
  Dcl-Ds UserSpaceHeader LikeDs(UserSpaceHeader_Ds) Based(UserSpaceDataPointer);
  Dcl-S UserSpaceEntryPointer Pointer;
  Dcl-Ds OBJL0100 Based(UserSpaceEntryPointer) Qualified;
    Name Char(10) Pos(1);
  End-Ds;
  Dcl-Ds JOBD0100 Len(20000) Qualified;
    LibLPos Int(10) Pos(361);
    LibCount Int(10) Pos(365);
  End-Ds;
  Dcl-S LibraryNamePointer Pointer;
  Dcl-S LibraryName Char(10) Based(LibraryNamePointer);
  Dcl-S LibraryOk Ind Dim(5);
  Dcl-S PrintInd Ind;

  // Create user space
  CreateUserSpace(UserSpaceName: Error);
  If (Error.BytesAvailable > 0); // Check error
    Close QSysPrt;
    EscapeMessage(Error);
    Return;
  Else;
    // List objects (type *JOBD in library)
    quslobj(UserSpaceName:'OBJL0100':'*ALL      '+Library:'*JOBD':Error);
    If (Error.BytesAvailable > 0); // Check error
      Close QSysPrt;
      EscapeMessage(Error);
      Return;
    EndIf;

    // Retrieve the user space pointer
    qusptrus(UserSpaceName:UserSpaceDataPointer:Error);
    If (Error.BytesAvailable > 0);
      Close QSysPrt;
      EscapeMessage(Error);
      Return;
    EndIf;

    // Check if libraries were found
    If (UserSpaceHeader.NumberOfEntries > 0);
      // Process all objects
      For k = 1 To UserSpaceHeader.NumberOfEntries;
        UserSpaceEntryPointer = UserSpaceDataPointer + UserSpaceHeader.OffsetListData + (k-1) * UserSpaceHeader.SizeOfEntry;
        
        // Retrieve job description information 
        qwdrjobd(JOBD0100: %Size(JOBD0100): 'JOBD0100': OBJL0100.Name + Library: Error);
        If (Error.BytesAvailable > 0); // Check error
          Close QSysPrt;
          EscapeMessage(Error);
          Return;
        EndIf;

        // If libraries defined in the job description
        If (JOBD0100.LibCount > 0);
          Clear LibraryOk;

          // Check all libreries
          j = 1;
          Dow (j <= 5);
            If (Libraries(j) <> *Blanks);
              LibraryNamePointer = %Addr(JOBD0100) + JOBD0100.LibLPos;
              i = 1;
              Dow (i <= JOBD0100.LibCount);
                If (LibraryName = Libraries(j));
                  LibraryOk(j) = *On;
                EndIf;
                If (i < JOBD0100.LibCount);
                  LibraryNamePointer += 11;            
                EndIf;
                i += 1;
              EndDo;
            Else;
              LibraryOk(j) = *On;
            EndIf;
            j += 1;
          EndDo;

          PrintInd = *Off;
          
          If (Match = '*ANY'); // Match *ANY
            j = 1;
            Dow ((PrintInd = *Off) And (j <= 5));
              If ((Libraries(j) <> *blanks) And (LibraryOk(j) = *On));
                PrintInd = *On;
              EndIf;
              j += 1;
            EndDo;
          ElseIf (Match = '*ALL'); // Match *ALL
            PrintInd = *On;
            j = 1;
            Dow ((PrintInd = *On) And (j <= 5));
              If ((Libraries(j) <> *blanks) And (LibraryOk(j) = *Off));
                PrintInd = *Off;
              EndIf;
              j += 1;
            EndDo;
          EndIf;
          
          // The job description entry should be printed into the list 
          If (PrintInd = *On);
            // Check if header should be printed 
            If (QSysPrtOverflow = *On);
              Write QSysPrt QSysPrtHeader1;
              Write QSysPrt QSysPrtHeader2;
              QSysPrtOverflow = *Off;
            EndIf;

            // Prepare the line
            QSysPrtLine = Library + ' ' + OBJL0100.Name;
            i = 0;
            For j = 1 To 5;
              If ((Libraries(j) <> *blanks) And (LibraryOk(j) = *On));
                i += 1;
                QSysPrtLine.Libs(i) = Libraries(j) + ' ';
              EndIf;
            EndFor;

            // Print the line
            Write QSysPrt QSysPrtLine;
          EndIf;
        EndIf;
      EndFor;
    EndIf;
  EndIf;

  // Delete user space
  DeleteUserSpace(UserSpaceName: Error);
End-Proc;

