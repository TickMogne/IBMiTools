**Free

//
// Tool0003
//
// Created by: github.com/TickMogne, 2022.05.02
//
// Convert IFS SQL stream file to *QMQRY File.
// See the command Tool0003.
//
// Compile parameters:
//  INCDIR('xxx/TMLib/src/') where xxx the parent library of the repository TMLib (github.com/TickMogne/TMLib)
//

Ctl-Opt DftActGrp(*No) Main(Tool0003) BndDir('TMLIB_M');

/include TMApi_Inc.rpgle
/include TMLib_Inc.rpgle

Dcl-Proc Tool0003;
  Dcl-Pi Tool0003;
    IfsFile Char(128);
    QMQRY Char(20);
  End-Pi;

  Dcl-S Library Char(10);
  Dcl-S File Char(10);
  Dcl-S File_io Pointer;
  Dcl-S Line Char(8192);
  Dcl-S LineLen Int(10);
  Dcl-S LineNum Int(10) Inz(0);
  Dcl-S LinePos Int(10);
  Dcl-S OutLine Char(8192);
  Dcl-S OutLineLen Int(10);
  Dcl-S OutPart Char(8192);
  Dcl-S OutPartLen Int(10);
  Dcl-S Info Char(80);
  Dcl-S Ret Pointer;
  Dcl-S i Int(10);
  Dcl-S TmpPtr Pointer;
  Dcl-S Command Char(128);
  Dcl-Ds Error LikeDs(ERRC0100);

  Library = %Subst(QMQRY: 11: 10);
  File = %Subst(QMQRY: 1: 10);

  Exec Sql SET OPTION COMMIT = *NONE;

  // Open stream file
  File_io = fopen(%Trim(IfsFile) + x'00': 'r');
  If (File_io = *Null); // Check error
    EscapeMessage(*Omit: 'File ' + %Trim(IfsFile) + ' cannot be opened');
  EndIf;

  // Delete temp files - if they exist
  Monitor;
    Command = 'DLTF FILE(QTEMP/QMQ1)';
    qcmdexc(Command: %Len(%Trim(Command)));
    Command = 'DLTF FILE(QTEMP/QMQ2)';
    qcmdexc(Command: %Len(%Trim(Command)));
  On-Error;
  EndMon;

  // Create temp files
  Monitor;
    Command = 'CRTPF FILE(QTEMP/QMQ1) RCDLEN(79)';
    qcmdexc(Command: %Len(%Trim(Command)));
    Command = 'CRTSRCPF FILE(QTEMP/QMQ2) MBR(*FILE)';
    qcmdexc(Command: %Len(%Trim(Command)));
  On-Error;
    EscapeMessage(*Omit: %Trim(ProgramStatus.ExceptionText));
  EndMon;

  // Read the stream file
  Dow (feof(File_io) = 0);
    // Read the next line
    Ret = fgets(%Addr(Line): %Size(Line): File_io);
    // If the read was successfull
    If (Ret <> *Null);
      LineNum += 1;
      LineLen = strlen(%Addr(Line));
      i = LineLen;
      // Remove the trailing special characters
      Dow ((i > 0) And ((%Subst(Line:i:1) = CHAR_CR) Or (%Subst(Line:i:1) = CHAR_LF) Or (%Subst(Line:i:1) = CHAR_NL) Or (%Subst(Line:i:1) = *blank)));
        %Subst(Line:i:1) = x'00';
        i -= 1;
        LineLen -= 1;
      EndDo;
      LinePos = 1;
      // Go to the first nonblank line position
      Dow ((LinePos <= LineLen) And (%Subst(Line: LinePos: 1) = *Blank));
        LinePos += 1;
      EndDo;
      If ((LinePos <= LineLen) And (%Subst(Line: LinePos: 2) <> '--'));
        // Write info about the original line
        Info = '-- original line nr. ' + %Char(LineNum);
        Exec Sql INSERT INTO qtemp/qmq1 VALUES(:Info);

        // Split the stream file line into shorter (max. 79 length) lines
        OutLine = *blanks;
        OutLineLen = 0;
        // Read until end of line
        Dow (LinePos <= LineLen);
          OutPart = *blanks;
          OutPartLen = 0;
          // Get the next text without blanks
          Dow ((LinePos <= LineLen) And (%Subst(Line: LinePos: 1) <> *Blank));
            OutPartLen += 1;
            %Subst(OutPart: OutPartLen: 1) = %Subst(Line: LinePos: 1);
            LinePos += 1;
          EndDo;
          // If text was found
          If (OutPartLen > 0);
            // If the length of the text greater than 79
            If (OutPartLen > 79);
              EscapeMessage(*Omit: 'The line number ' + %Char(LineNum) + ' can''t be splitted to a valid source file line.' +
                ' Please use some possible spaces to split the expressions into text parts with maximal length of 79.');
            EndIf;
            // If the length of the line with the text greater than 79, write the line without the text
            If ((OutLineLen + OutPartLen) > 79);
              Exec Sql INSERT INTO qtemp/qmq1 VALUES(:OutLine);
              OutLine = *blanks;
              OutLineLen = 0;
            EndIf;
            // Append the text to the line
            TmpPtr = memcpy(%Addr(OutLine) + OutLineLen: %Addr(OutPart): OutPartLen + 1);
            OutLineLen += (OutPartLen + 1);
          EndIf;
          LinePos = LinePos + 1;
        EndDo;
        // If there is a text to write
        If (OutLineLen > 0);
          Exec Sql INSERT INTO qtemp/qmq1 VALUES(:OutLine);
        EndIf;
      EndIf;
    EndIf;
  EndDo;

  // Close the stream file
  fclose(File_io);

  // Copy the lines into the source file
  Command = 'CPYF FROMFILE(QTEMP/QMQ1) TOFILE(QTEMP/QMQ2) MBROPT(*REPLACE) FMTOPT(*CVTSRC)';
  qcmdexc(Command: %Len(%Trim(Command)));

  // Delete the QMQRY file - if it exists
  Monitor;
    Command = 'DLTQMQRY QMQRY(' + %Trim(Library) + '/' + %Trim(File) + ')';
    qcmdexc(Command: %Len(%Trim(Command)));
  On-Error;
  EndMon;

  // Create the QMQRY file
  Command = 'CRTQMQRY QMQRY(' + %Trim(Library) + '/' + %Trim(File) + ') SRCFILE(QTEMP/QMQ2) SRCMBR(QMQ2)';
  qcmdexc(Command: %Len(%Trim(Command)));

  // Delete temp files
  Command = 'DLTF FILE(QTEMP/QMQ1)';
  qcmdexc(Command: %Len(%Trim(Command)));
  Command = 'DLTF FILE(QTEMP/QMQ2)';
  qcmdexc(Command: %Len(%Trim(Command)));
End-Proc;
