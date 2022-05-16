**Free

//
// Tool0008a
//
// Created by: github.com/TickMogne, 2022.05.15
//
// Exit program for FTP server request.
//
// Compile parameters:
//  INCDIR('xxx/TMLib/src/') where xxx the parent library of the repository TMLib (github.com/TickMogne/TMLib)
//

// CREATE TABLE QGPL.FTPLOG ( 
// 	DATETIME TIMESTAMP, 
// 	APP_ID INTEGER, 
// 	OP_ID INTEGER DEFAULT NULL, 
// 	"USER" VARCHAR(32) CCSID 1141, 
// 	REMOTE_IP VARCHAR(32) CCSID 1141, 
// 	INFO VARCHAR(1024) DEFAULT NULL CCSID 1141)   
// 	RCDFMT FTPLOGR;

// ADDEXITPGM EXITPNT(QIBM_QTMF_SERVER_REQ)
//            FORMAT(VLRQ0100)             
//            PGMNBR(*LOW)                 
//            PGM(QGPL/TOOL0008A)         

Ctl-Opt DftActGrp(*No) Main(Tool0008a) BndDir('TMLIB_M');

/include TMApi_Inc.rpgle
/include TMLib_Inc.rpgle

// Main procedure
Dcl-Proc Tool0008a;
  Dcl-Pi Tool0008a;
    ApplicationIdentifier Int(10);
    OperationIdentifier Int(10);
    UserProfile Char(10);
    RemoteIPAddress Char(128) Options(*Varsize);
    RemoteIPAddressLength Int(10);
    OperationSpecificInformation Char(1024) Options(*Varsize);
    OperationSpecificInformationLength Int(10);
    AllowOperation Int(10);
  End-Pi;
  Dcl-Ds Log ExtName('QGPL/FTPLOG') Qualified End-Ds;

  Exec Sql SET OPTION COMMIT = *NONE;

  Log.datetime = %Timestamp();
  Log.app_id = ApplicationIdentifier;
  Log.op_id = OperationIdentifier;
  Log.user = UserProfile;
  Log.remote_ip = %Subst(RemoteIPAddress: 1: RemoteIPAddressLength);
  Log.info = %Subst(OperationSpecificInformation: 1: OperationSpecificInformationLength);

  Exec Sql INSERT INTO qgpl.ftplog
    VALUES(:Log);  

  AllowOperation = 1;

End-Proc;
