//---------------------------------------------------------------------------
//
//  Program:     UnitFileCentral is part of MidiAndMusicXmlPlayerTest.exe
//
//  Project:     This is a test program for MidiAndMusicXmlPlayer.exe
//
//  Purpose:     UnitFileCentral finds the names of all xml-file and
//               sends a message via the PlayStart-semaphore to start
//               processing in UnitPlayProcess
//
//  Compilation: Compile with Delphi 4 (MidiAndMusicXmlPlayerTest.dpr)
//
//  Description: The WIN32 functions findfirst and findnext are used
//               to retrieve all file names. The semaphore PlayTicket
//               limits the number of Play-processes. This number should
//               be the same as the number of kernels in the CPU.
//
//---------------------------------------------------------------------------

unit UnitFileCentral;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes,            // TThread
  SysUtils,           // findfirst, findnext
  UnitSemaphores;     // send, receive etc.

type
  TFileCentral = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  end;

implementation

{ TFileCentral }

const NumberOfTickets=16;

//---------------------------------------------------------------------------
//
//     Procedure:  Execute
//
//     Purpose:    To run the thread process, i.e. run FileCentral
//
//     Parameters: Sender = Windows standard
//
//     Returns:    void
//
//     Notes:      Runs forever waiting for commands.
//
//---------------------------------------------------------------------------

procedure TFileCentral.Execute;

var SemReturn: longint;
    SemMessage: string;
    SemDummy: longint;
    SearchRecord: TSearchRec;
    Command: string;
    i: integer;

  begin
  while true do
    begin
    // Receive start message and start time counting and file counting
    TimeRunning:=false;
    Semaphore.Receive(FileCentralStart,SemReturn,SemMessage);
    TimeRunning:=true;
    FileNumber:=0;
    FileNumberFinished:=0;
    // When all the tickets are used, the FileCentral-process waits
    // until some PlayProcess-process terminates and returns a ticket
    for i:=1 to NumberOfTickets do Semaphore.Send(PlayTicket,NoReturn,IntToStr(i));
    // Search first xml-file
    if findfirst('*.xml',faAnyFile,SearchRecord)=0 then
      begin
      Semaphore.Receive(PlayTicket,SemDummy,SemMessage);
      inc(FileNumber);
      Command:='MidiAndMusicXmlPlayer "'+SearchRecord.Name+'" -T';
      Semaphore.Send(PlayStart,PlayTicket,PChar(command));
      Semaphore.Send(PlayLog,NoReturn,PChar(command));
      // Search the rest of the xml-files
      while findnext(SearchRecord)=0 do
        begin
        Semaphore.Receive(PlayTicket,SemReturn,SemMessage);
        inc(FileNumber);
        Command:='MidiAndMusicXmlPlayer "'+SearchRecord.Name+'" -T';
        Semaphore.Send(PlayStart,PlayTicket,PChar(command));
        Semaphore.Send(PlayLog,NoReturn,PChar(command));
        end;
      end;
    // Wait for the last processes to finish
    for i:=1 to NumberOfTickets do
      Semaphore.Receive(PlayTicket,SemDummy,SemMessage);
    TimeRunning:=false;
    // Start also the compare program
    Command:='"C:\Program Files (x86)\WinMerge\WinMergeU.exe" . ..\org';
    Semaphore.Send(PlayStart,NoReturn,Command);
    Semaphore.Send(PlayLog,NoReturn,PChar(command));
    Semaphore.Send(PlayLog,NoReturn,'Test is ready');
    end;
  end;

end.


