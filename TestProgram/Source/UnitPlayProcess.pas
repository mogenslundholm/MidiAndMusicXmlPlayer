//---------------------------------------------------------------------------
//
//  Program:     UnitPlayProcess is part of MidiAndMusicXmlPlayerTest.exe
//
//  Project:     This is a test program for MidiAndMusicXmlPlayer.exe
//
//  Purpose:     PlayProcess is a thread to run one instance of
//               MidiAndMusicXmlPlayer.
//
//  Compilation: Compile with Delphi 4 (MidiAndMusicXmlPlayerTest.dpr)
//
//  Description: Two kind of threads are defined: FileCentral and PlayProgram
//               in units UnitFileCentral and Unit PlayProgram
//
//---------------------------------------------------------------------------

unit UnitPlayProcess;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, Windows,
  UnitSemaphores;

type
  TPlayProcess = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  end;

implementation

var
  StartInfo  : TStartupInfo;
  ProcInfo   : TProcessInformation;
  CreateOK   : Boolean;
  SemReturn: longint;
  SemMessage: string;

{ TPlayProcess }

//---------------------------------------------------------------------------
//
//     Procedure:  Execute
//
//     Purpose:    To run the thread process, i.e. run the player program
//
//     Parameters: Sender = Windows standard
//
//     Returns:    void
//
//     Notes:      Runs forever waiting for commands.
//
//---------------------------------------------------------------------------

procedure TPlayProcess.Execute;

  begin
  while true do
    begin
    // Wait for a message from FileCentral with a command with file name
    Semaphore.Receive(PlayStart,SemReturn,SemMessage);
    // Current file is saved to show in the status line
    CurrentFile:=SemMessage;
    CreateOk:=CreateProcess(nil,PChar(SemMessage),nil, nil,False,
                      CREATE_NEW_PROCESS_GROUP+NORMAL_PRIORITY_CLASS,
                      nil,nil,StartInfo,ProcInfo);
    if CreateOk then
      begin
      WaitForSingleObject(ProcInfo.hProcess, INFINITE);
      end;
    // The process has terminated - send message back
    Semaphore.Send(SemReturn,NoReturn,SemMessage);
    Semaphore.Send(PlayReturn,NoReturn,SemMessage);
    // Also Winmerge call comes here but not counted as xml-file
    end;
   end;

end.
