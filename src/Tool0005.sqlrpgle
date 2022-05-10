**Free

// CRTDTAARA DTAARA(QGPL/TOOL0005)
//           TYPE(*CHAR)          
//           LEN(20)              

// create table qgpl.tool0005log
//  (f_timestamp timestamp not null,
//   f_jobname char(10) not null,
//   f_username char(10) not null,
//   f_jobnumber char(6) not null,
//   f_library char(10) not null,
//   f_file char(10) not null,
//   f_member char(10) not null,
//   f_interface char(1) not null,
//   f_underlying char(1) not null,
//   f_input char(1) not null,
//   f_output char(1) not null,
//   f_update char(1) not null,
//   f_delete char(1) not null
//  )

// ADDEXITPGM EXITPNT(QIBM_QDB_OPEN)
//            FORMAT(DBOP0100)      
//            PGMNBR(*LOW)          
//            PGM(QGPL/TOOL0005)   

Ctl-Opt DftActGrp(*No) Main(Tool0005) BndDir('TMLIB_M');

/include TMApi_Inc.rpgle
/include TMLib_Inc.rpgle

Dcl-Ds Config DtaAra('TOOL0005') Qualified;
  LibraryName Char(10);
  FileName Char(10);
End-Ds;

// Main procedure
Dcl-Proc Tool0005;
  Dcl-Pi Tool0005;
    Information LikeDs(DBOP0100);
    ReturnCode Int(10);
  End-Pi;
  Dcl-Ds DBOP0100 Len(65535) Qualified Template;
    FileArrayOffset Int(10) Pos(13);
    FileArrayEntryCount Int(10) Pos(17);
    FileArrayEntrySize Int(10) Pos(21);
    JobName Char(10) Pos(25);
    UserName Char(10) Pos(35);
    JobNumber Char(6) Pos(45);
    Interface Char(1) Pos(61);
  End-Ds;
  Dcl-S FileArrayEntryPointer Pointer;
  Dcl-Ds FileArrayEntry Len(44) Based(FileArrayEntryPointer) Qualified;
    FileName Char(10) Pos(1);
    LibraryName Char(10) Pos(11);
    MemberName Char(10) Pos(21);
    Underlying Int(10) Pos(37);
    Input Char(1) Pos(41);
    Output Char(1) Pos(42);
    Dcl-Subf Update Char(1) Pos(43);
    Dcl-Subf Delete Char(1) Pos(44);
  End-Ds;
  Dcl-S i Int(10);
  Dcl-Ds Log Qualified;
    f_timestamp Timestamp;
    f_jobname Char(10);
    f_username Char(10);
    f_jobnumber Char(6);
    f_library Char(10);
    f_file Char(10);
    f_member Char(10);
    f_interface Char(1);
    f_underlying char(1);
    f_input Char(1);
    f_output Char(1);
    f_update Char(1);
    f_delete Char(1);
  End-Ds;

  Exec Sql SET OPTION COMMIT = *NONE;

  In Config;

  FileArrayEntryPointer = %Addr(Information) + Information.FileArrayOffset;
  For i = 1 To Information.FileArrayEntryCount;
    If ((FileArrayEntry.LibraryName = Config.LibraryName) And (FileArrayEntry.FileName = Config.FileName));
      // Build the log entry
      Log.f_timestamp = %Timestamp();
      Log.f_jobname = Information.JobName;
      Log.f_username = Information.UserName;
      Log.f_jobnumber = Information.JobNumber;
      Log.f_library = FileArrayEntry.LibraryName;
      Log.f_file = FileArrayEntry.FileName;
      Log.f_member = FileArrayEntry.MemberName;
      Log.f_interface = Information.Interface;
      If (FileArrayEntry.Underlying = 1);
        Log.f_underlying = '1';
      Else;
        Log.f_underlying = '0';
      EndIf;
      Log.f_input = FileArrayEntry.input;
      Log.f_output = FileArrayEntry.output;
      Log.f_update = FileArrayEntry.update;
      Log.f_delete = FileArrayEntry.delete;

      // Insert the log entry
      Exec Sql INSERT INTO qgpl.tool0005log VALUES(:Log.f_timestamp, :Log.f_jobname, :Log.f_username, :Log.f_jobnumber,
        :Log.f_library, :Log.f_file, :Log.f_member, :Log.f_interface, :Log.f_underlying, :Log.f_input, :Log.f_output,
        :Log.f_update, :Log.f_delete);

      Leave;
    EndIf;
    FileArrayEntryPointer += Information.FileArrayEntrySize;
  EndFor;

  // The open request should be accepted
  ReturnCode = 1;
End-Proc;


