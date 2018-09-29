//---------------------------------------------------------------------------
//
//  Program:     UnitMidiPlay.pas in MidiAndMusicXml.exe
//
//  Project:     MidiAndMusicXml.exe and MidiAndMusicXml.app
//
//  Purpose:     To play the midi events by sending the appropriate
//               midi-events to the midi-interface using Win32 calls
//
//  Compilation: Compile with Delphi 4 (Midi.dpr) or Lazarus (Midi.lpr)
//               or Delphi XE (Midi.dproj)
//
//  Description: This program is a thread running in parallell with the
//               graphical user interface (UnitMidiGui)
//
//---------------------------------------------------------------------------

unit UnitMidiPlay;

{$ifdef FPC}
{$MODE Delphi}
{$endif}
interface

uses
  Classes,
{$ifdef Windows}
  Windows,
  MMSystem,
  UnitMidiIn,
{$endif}
  SysUtils,
  UnitMidiDefinitions;

{$ifdef Darwin}
{$linkframework CoreAudio}
{$linkframework CoreMidi}
{$linkframework CoreAudioKit}
{$linkframework AudioToolbox}
{$linkframework AudioUnit}
{$linkframework Carbon}
{$linkframework CoreServices}
{$linkframework QuickTime}
{$LINKLIB libmidilib.a}
procedure MidiOutReset(MidiSelector: PChar); cdecl; external;
procedure MidiOutShortMsg(byte1: byte; byte2: byte; byte3: byte; byte4: byte); cdecl; external;
function MidiOutLongMsg(var bytes: array of byte; len: uint32): int32; cdecl; external;
procedure MidiDelta(delta: uint32); cdecl; external;
{$endif}

type
  MidiPlay = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  end;

procedure MidiTimeSetEvent(milliseconds: int64);
procedure MidiOutMsg(dw: DWord);
{$ifdef Windows}
procedure MidiInCallBack(MidiInHandle: THandle; M: TMidiShortMessage;
          Instance: DWORD; Param1: DWORD; Param2: DWORD);  stdcall;
{$endif}

implementation

// Collect events in this time span (milleseconds).
// (Note: In Windows 7 and Windows 8 System calls takes long time)
{$ifdef Darwin}
const TimeSpan: int64 = 0;
{$else}
const TimeSpan: int64 = 20;
{$endif}

var OldVolume: int64;
    Volume: int64;

const FirstTimeIn: boolean = true;
const FirstTimeOut: boolean = true;

var    TestData: textfile;  // A file for testdata instead of running

{ Important: Methods and properties of objects in VCL can only be used in a
  method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure MidiPlay.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ MidiPlay }

{$ifdef DelphiXe}
type UINT = uint32;
{$endif}

//---------------------------------------------------------------------------
//
//     Function:  MidiStopAllNotes
//
//     Purpose:   Mac uses this function (instead of PC's MidiOutReset)
//                to stop all notes.
//
//     Parameters: none
//
//     Returns:    void
//
//     Notes:      none
//
//---------------------------------------------------------------------------


procedure MidiStopAllNotes;

var i,j: integer;
    MidiShortMessage: TMidiShortMessage;
  begin
  for i:=0 to Channels do
  for j:=0 to MidiNoteLast do
    begin
    MidiShortMessage.Bytes[0]:=($80+i-1);
    MidiShortMessage.Bytes[1]:=(j);
    MidiShortMessage.Bytes[2]:=0;
    MidiShortMessage.Bytes[3]:=0;
    MidiOutMsg(MidiShortMessage.Word);
    end;
  end;


//---------------------------------------------------------------------------
//
//     Function:  MidiOutCallbackFunction
//
//     Purpose:   Call back function to play one or more notes and set new timer
//
//     Parameters: MMSystem defined parameters (not used)
//
//     Returns:    void
//
//     Notes:      Activated by time out - play new note and set timer again
//
//---------------------------------------------------------------------------



procedure MidiOutCallbackFunction(id, msg : UINT; player : DWORD; dw1, dw2 : DWORD); stdcall;

var
  Channel: integer;
  MidiShortMessage: TMidiShortMessage;
  MidiShortMessagePanorama: TMidiShortMessage;
  i: integer;
  Milliseconds: int64;
  VolumeVal: int64;

  begin
  if FirstTimeOut then
    begin
    if MidiTest then
      begin
      AssignFile(TestData,FileName+'.testdata');
      rewrite(TestData);
      writeln(TestData,LowerCase(UserProfile));
      end;
    FirstTimeOut:=false;
    end;

  if not MidiTest then
    begin
    OldVolume:=Volume;
    Volume:=(MidiDataMixer[0]*$FFFF) div 127;
    if Volume>$FFFF then Volume:=$FFFF
    else if Volume<0 then Volume:=0;

    VolumeVal:=Volume+(Volume shl 16);
{$ifdef Windows}
    if Volume<>OldVolume then
//// Windows-makro dur vist ikke
      MidiOutSetVolume(0,VolumeVal);
{$endif}
    end;
  MilliSeconds:=0;

    repeat
    // Substitute ?
    MidiShortMessage:=MidiEvents[MidiEventIndexOut];

    // Note on          ///// Brug shortmessage !!!!
    if (ord(MidiEvents[MidiEventIndexOut].Bytes[0]) and $F0)=$90 then
      begin
      Channel:=((MidiShortMessage.Bytes[0]) and $F)+1;
      if not MidiProgramSet[Channel] then
        begin
        // No instrument is set
        //// do something
        ////qqqqqqqqqq       se heilaga.mid!
        end;

      // Use MidiFirstNote to avoid pause before playing
      if MidiFirstNote and MidiDataChecked[Channel] then
        begin
        MilliSeconds:=0;
        MidiFirstNote:=false;
        end;

      //// TEST set mixer value
      Volume:=(MidiShortMessage.Bytes[2]*MidiDataMixer[Channel]
               *MidiDataMixer[0]) div 4096;
      if Volume>127 then Volume:=127
      else if Volume<0 then Volume:=0;

      MidiShortMessage.Bytes[2]:=Volume;

      if not MidiDataChecked[Channel] then
        begin
        MidiShortMessage.Bytes[0]:=
          (ord(MidiShortMessage.Bytes[0])-$10);
{$ifdef FPC}
        if Channel<>10 then
{$else}
        if (MidiTranspose<>0) and (Channel<>10) then
{$endif}
          begin // All notes except drums are raised with number of halfsteps
          MidiShortMessage.Bytes[1]:=
             (ord(MidiShortMessage.Bytes[1])+MidiTranspose);
          end
        end
      else if MidiDataRhythm[Channel] then
        begin // Change to rhythm instrument
////        while true do begin end;;    HVAD ER DETTE?????
////        MidiShortMessage.Bytes[0]:=chr($9A);
////        MidiShortMessage.Bytes[2]:=chr(52);
        end

      else if (MidiTranspose<>0) and (Channel<>10) then
        begin // All notes except drums are raised with number of halfsteps
        i:=ord(MidiShortMessage.Bytes[1]);
        i:=i+MidiTranspose;
        if (i>=0) and (i<=127) then
          MidiShortMessage.Bytes[1]:=i
        else if i>127 then
          MidiShortMessage.Bytes[1]:=127
        else
          MidiShortMessage.Bytes[1]:=0;
        end;
      end
    else if (ord(MidiEvents[MidiEventIndexOut].Bytes[0]) and $F0)=$80 then
      begin
      if MidiTranspose<>0 then
        begin
        i:=ord(MidiShortMessage.Bytes[1]);
        i:=i+MidiTranspose;
        if (i>=0) and (i<=127) then
          MidiShortMessage.Bytes[1]:=i
        else if i>127 then
          MidiShortMessage.Bytes[1]:=127
        else
          MidiShortMessage.Bytes[1]:=0;
         // Stop also eventually old notes //// !!!! fel
        MidiOutMsg(MidiEvents[MidiEventIndexOut].Word);
        end;
      end
    else if (ord(MidiShortMessage.Bytes[0]) and $F0)=$B0 then
      begin
      if ord(MidiShortMessage.Bytes[1])=10 then
        begin
        Channel:=(ord(MidiShortMessage.Bytes[0]) and $F)+1;
        MidiShortMessage.Bytes[2]:=
          MidiDataPanorama[Channel];
        end;
      end
    else if (ord(MidiShortMessage.Bytes[0]) and $F0)=$C0 then
      begin
      Channel:=((MidiShortMessage.Bytes[0]) and $F)+1;
      MidiDataInstrumentOrg[Channel]:=MidiShortMessage.Bytes[1];
      if ord(MidiDataComboBoxValue[Channel])<128 then
        MidiShortMessage.Bytes[1]:=MidiDataComboBoxValue[Channel]
      else
        MidiShortMessage.Bytes[1]:=0;
      end
    else if (ord(MidiShortMessage.Bytes[0]) and $F0)=$0 then
    // Code used for tempo change (0 - such value should not occur)
      begin
      if MidiShortMessage.Bytes[1]=SpecialMetaEventTempo then
        MidiTempo:=MidiTempos[MidiShortMessage.Bytes[2]]
      else if MidiShortMessage.Bytes[1]=SpecialMetaEventMeasure then
        MidiMeasureNumberDisplay:=(MidiShortMessage.Bytes[2] shl 8)+MidiShortMessage.Bytes[3];
      end;
    for i:=1 to Channels do
      begin
      if MidiDataPanoramaChanged[i] then
        begin // Panorama value (right-left)
        MidiShortMessagePanorama.Bytes[0]:=$B0+i-1;
        MidiShortMessagePanorama.Bytes[1]:=10;
        MidiShortMessagePanorama.Bytes[2]:=MidiDataPanorama[i];
        MidiShortMessagePanorama.Bytes[3]:=0;
        MidiOutMsg(MidiShortMessagePanorama.Word);

        dec(MidiDataPanoramaChanged[i]);
        end;
      if MidiDataComboBoxChanged[i] then
        begin // Set instrument according to selected value
        MidiShortMessagePanorama.Bytes[0]:=$C0+i-1;
        MidiShortMessagePanorama.Bytes[1]:=MidiDataComboBoxValue[i];
        MidiShortMessagePanorama.Bytes[2]:=0;
        MidiShortMessagePanorama.Bytes[3]:=0;
        MidiOutMsg(MidiShortMessagePanorama.Word);

        MidiDataComboBoxChanged[i]:=false;
        end
      else if not MidiProgramSet[i] then
        begin // Set piano because the command is missing
        MidiShortMessagePanorama.Bytes[0]:=$C0+i-1;  //// Byt navn, ikke panarama
        MidiShortMessagePanorama.Bytes[1]:=0;  // Piano
        MidiShortMessagePanorama.Bytes[2]:=0;  // Not used
        MidiShortMessagePanorama.Bytes[3]:=0;  // Not used
        MidiOutMsg(MidiShortMessagePanorama.Word);

        MidiDataComboBoxValue[i]:=0;
        MidiProgramSet[i]:=true;
        end;
      end;

    // Filter the messages of B0 ////
{$ifdef Darwin}
    if ((MidiShortMessage.Bytes[0] and $F0)<>$B0) or
      (MidiShortMessage.Bytes[1]<>$20)
    then
{$endif}
    MidiOutMsg(MidiShortMessage.Word);
    inc(MidiEventIndexOut);
    MidiDeltaTotalOut:=MidiDeltaTotalOut+MidiEventDelta[MidiEventIndexOut];

    // Compute delay before next note
////    if MidiDivision=0 then MidiDivision:=1;
    MilliSeconds:=
            MilliSeconds+
           (MidiEventDelta[MidiEventIndexOut]*MidiTempo) div
           (MidiDivision*10*MidiTempoProcent);

    until ((MilliSeconds>TimeSpan) and (not MidiFirstNote)) or
            (MidiEventIndexOut>=MidiEventIndexIn);

  if (MidiTest or MidiAutoClose) and (MidiEventIndexOut>=MidiEventIndexIn) then
    begin
    if not MidiShallClose then CloseFile(TestData);
    // Also save midi and midi decoding
    SaveMidiData;
    FormMidiIn.MemoMidiIn.Lines.SaveToFile(FileName+'.decoding.txt');
    MidiShallClose:=true;
    end;

  if (MidiEventIndexIn>0) and (MidiDeltaTotalIn>0) then
    begin
    MidiProgress:=(MidiDeltaTotalOut*200) div MidiDeltaTotalIn;    //// ????
    if MidiPositionForward<MidiProgress then MidiPositionForward:=-1;
    end
  else
    begin
    MidiProgress:=0;
    end;

  // Cycle?
  if (SystemState=MidiPlaying) and(MidiCycleStartMark>=0) and (MidiCycleEndMark>=0) then
    begin
    if MidiEventIndexOut>MidiCycleEndMark then
      begin
      MidiFirstNote:=true;
      MidiEventIndexOut:=MidiCycleStartMark;
      MidiDeltaTotalOut:=MidiCycleStartTime;
{$ifdef Windows}
      MidiStopAllNotes;
{$endif}
{$ifdef Darwin}
      // The MAC seems to need some time before ready
      MidiStopAllNotes;
      MidiDelta(200);
{$endif}
      Milliseconds:=1;
      MidiTempo:=MidiCycleStartTempo;
      end;
    end;

  if MidiPositionChange then
    begin
    MidiPositionChange:=false;
    if SystemState=MidiPlaying then SystemState:=MidiPositioning;
    MidiEventIndexOut:=0;
    MidiDeltaTotalOut:=0;
    MidiPositionForward:=MidiPositionForwardNext;
    MidiStopAllNotes;
    end;

  if MidiSelect<>MidiSelectNew then
    begin
    MidiStopAllNotes;
    MidiSelect:=MidiSelectNew;
    // Rerun control messages
    for i:=0 to MidiEventIndexOut do
      begin
      MidiShortMessage:=MidiEvents[i];
      // Note on
      if (ord(MidiShortMessage.Bytes[0]) and $F0)<>$90 then
      if (ord(MidiShortMessage.Bytes[0]) and $F0)<>$80 then
        begin
        MidiOutMsg(MidiShortMessage.Word);
        end
      end;
    // Set instrument again (New synth means forgotten instrument)
    for i:=1 to Channels do
      begin // Panorama
      MidiShortMessagePanorama.Bytes[0]:=$B0+i-1;
      MidiShortMessagePanorama.Bytes[1]:=10;
      MidiShortMessagePanorama.Bytes[2]:=MidiDataPanorama[i];
      MidiShortMessagePanorama.Bytes[3]:=0;
      MidiOutMsg(MidiShortMessagePanorama.Word);
      // (Reuse variable for instrument)
      MidiShortMessagePanorama.Bytes[0]:=$C0+i-1;
      MidiShortMessagePanorama.Bytes[1]:=MidiDataComboBoxValue[i];
      MidiShortMessagePanorama.Bytes[2]:=0;
      MidiShortMessagePanorama.Bytes[3]:=0;
      MidiOutMsg(MidiShortMessagePanorama.Word);
      end;
    end;

  if ((SystemState=MidiPlaying) or (SystemState=MidiPositioning)) and
//// Check this
{$ifdef FPC}
     (MidiEventIndexOut<MidiEventIndexIn) then
{$else}
     (MidiEventIndexOut<=MidiEventIndexIn) then
{$endif}
    begin
    MidiTimerRunning:=true;
    MidiTimeSetEvent(MilliSeconds);
    end
  else if SystemState=MidiPaused then
    begin
    MidiTimerRunning:=false;
    MidiStopAllNotes;
    end
  else
    begin
    MidiTimerRunning:=false;
    MidiStopAllNotes; //// Should be stopped allready
{$ifdef FPC}
    SystemState:=MidiUp;
{$endif}
{$ifdef Darwin}
    MidiOutReset;
{$endif}
    end;
  end;

{$ifndef Darwin}
//---------------------------------------------------------------------------
//
//     Function:  MidiInCallbackFunction
//
//     Purpose:   Call back function to receive midi input
//                Let the pedal stop (pause) and start playing
//
//     Parameters: MMSystem defined parameters (not used)
//
//     Returns:    void
//
//     Notes:      Activated by Midi-input
//
//---------------------------------------------------------------------------

procedure MidiInCallBack(MidiInHandle: THandle; M: TMidiShortMessage;
          Instance: DWORD; Param1: DWORD; Param2: DWORD);  stdcall;

var
  MidiOut: TMidiShortMessage;
  MidiInstrument: TMidiShortMessage;

  begin
  if FirstTimeOut then
    begin
    if MidiTest then
      begin
      AssignFile(TestData,FileName+'.testdata');
      rewrite(TestData);
      writeln(TestData,LowerCase(UserProfile));
      end;
    FirstTimeOut:=false;
    end;

  if FirstTimeIn then
    begin
    FirstTimeIn:=false;
    MidiInstrument.Bytes[0]:=$CF;
    MidiInstrument.Bytes[1]:=0;
    MidiInstrument.Bytes[2]:=$0;
    MidiInstrument.Bytes[3]:=$0;
    MidiOutMsg(MidiInstrument.Word);

    MidiInstrument.Bytes[0]:=$BF;
    MidiInstrument.Bytes[1]:=$0;
    MidiInstrument.Bytes[2]:=$0;
    MidiInstrument.Bytes[3]:=$0;
    MidiOutMsg(MidiInstrument.Word);

    MidiInstrument.Bytes[0]:=$0;
    MidiInstrument.Bytes[1]:=$1;
    MidiInstrument.Bytes[2]:=$0;
    MidiInstrument.Bytes[3]:=$0;
    MidiOutMsg(MidiInstrument.Word);

    MidiInstrument.Bytes[0]:=$BF;
    MidiInstrument.Bytes[1]:=$07;
    MidiInstrument.Bytes[2]:=$7F;
    MidiInstrument.Bytes[3]:=$0;
    MidiOutMsg(MidiInstrument.Word);

    MidiInstrument.Bytes[0]:=$BF;
    MidiInstrument.Bytes[1]:=$0A;
    MidiInstrument.Bytes[2]:=$7F;
    MidiInstrument.Bytes[3]:=$0;
    MidiOutMsg(MidiInstrument.Word);
    end;

  ////
  if (Param1 and $FFFFFF)=$7F0BB0 then //// Uden at trykke ned
  MidiOut.Word:=Param1
  else if (Param1 and $FFFFFF)=$7E0BB0 then
  MidiOut.Word:=Param1
  else if (Param1 and $FFFFFF)=$610BB0 then //// Ned 1
  MidiOut.Word:=Param1
  else if (Param1 and $FFFFFF)=$080BB0 then //// Ned a
  MidiOut.Word:=Param1
  else if (Param1 and $FFFFFF)=$080BB0 then //// Ned a
  MidiOut.Word:=Param1
  else if (Param1 and $FFFFFF)=$620BB0 then //// Ned 2
  MidiOut.Word:=Param1
  else if (Param1 and $FFFFFF)=$630BB0 then //// Ned 1
  MidiOut.Word:=Param1
  else if (Param1 and $FFFFFF)=$680BB0 then
  MidiOut.Word:=Param1
  else if (Param1 and $FFFFFF)<>$0 then
  MidiOut.Word:=Param1;

  if ((Param1 and $FFFF)=$0BB0) then
    begin

    if (Param1 and $FF0000)>$600000 then
      PedalDown:=PedalDown;

    PedalDownOld:=PedalDown;
    PedalDown:=((Param1 and $FF0000)<$200000) and (not((Param1 and $FF0000)>$600000));

    if PedalDown<>PedalDownOld then
      PedalDown:=PedalDown;

    if (PedalDown<>PedalDownOld) and PedalDown then
      PedalDownChanged:=true;
    end;

  if (MidiOut.Bytes[0] and $0F)<>$09 then
  MidiOut.Bytes[0]:=MidiOut.Bytes[0] or $0F;
  MidiOutMsg(MidiOut.Word);
  end;

{$endif}

//---------------------------------------------------------------------------
//
//     Function:  Execute
//
//     Purpose:   Starts the new process (thread)
//
//     Parameters: none
//
//     Returns:    void
//
//     Notes:      //// Started from ....????
//
//---------------------------------------------------------------------------

procedure MidiPlay.Execute;

{$ifdef UNIX}
var
  CreateStatus : Boolean;
  MidiPlayThread   : TThreadId;
{$endif}

var i: integer;

{$ifdef UNIX}
  begin
  MidiOutReset;
  MidiStopAllNotes;
  MidiDelta(300);
  MidiPlayThread:=GetCurrentThreadID;
  ThreadSetPriority(MidiPlayThread,15);
  if MidiTest then
    begin
    AssignFile(TestData,FileName+'.testdata');
    rewrite(TestData);
    writeln(TestData,LowerCase(UserProfile));
    end;
  OldVolume:=-1;
  Volume:=0;

  MidiEventIndexOut:=0;
  while true do
    begin
    if (SystemState=MidiPlaying) or (SystemState=MidiPositioning) then
      begin
      MidiOutCallbackFunction(0,0,0,0,0);
      end
    else
      begin
      MidiStopAllNotes;
      MidiDelta(200);
      end;
    end;
  if MidiTest then CloseFile(TestData);
  end;
{$else}
    begin

  if FirstTimeOut then
  begin
  if MidiTest then
    begin
    AssignFile(TestData,FileName+'.testdata');
    rewrite(TestData);
    writeln(TestData,LowerCase(UserProfile));
    end;
  FirstTimeOut:=false;
  end;


  for i:=0 to MidiOutNumDevs-1 do MidiOutReset(MidiOutHandle[i]);
  OldVolume:=-1;
  Volume:=0;
{$ifdef Windows}
  if MidiTest then
    Setpriorityclass(GetCurrentProcess(), IDLE_PRIORITY_CLASS)
  else
    Setpriorityclass(GetCurrentProcess(), REALTIME_PRIORITY_CLASS);
{$endif}
    begin
    SystemState:=MidiPlaying;
    // Start call back. Make a short delay of 200 milliseconds
    if not MidiTimerRunning then
      begin
      MidiTimerRunning:=true;
{$ifdef Windows}
      TimeSetEvent(200,1,@MidiOutCallBackFunction,DWORD(self),TIME_ONESHOT);
{$else}
      MidiDelta(200);
{$endif}
      end;
    end;
    end;
{$endif}


procedure MidiTimeSetEvent(milliseconds: int64);

  begin
  if (not MidiShallClose) and MidiTest then
    begin
    writeln(TestData,'Time: '+IntToStr(milliseconds));
{$ifdef Windows}
    TimeSetEvent(1,1,@MidiOutCallBackFunction,0,TIME_ONESHOT);
{$else}
      MidiDelta(1);
{$endif}
    end
  else if (SystemState=MidiPositioning) then
    begin
    if  MidiPositionForward<MidiProgress then
      SystemState:=MidiPlaying;
{$ifdef Windows}
    TimeSetEvent(1,1,@MidiOutCallBackFunction,0,TIME_ONESHOT);
{$else}
    MidiDelta(1);
{$endif}
    end
  else
{$ifdef Windows}
    TimeSetEvent(milliseconds,1,@MidiOutCallBackFunction,0,TIME_ONESHOT);
{$else}
    MidiDelta(milliseconds);
{$endif}
  end;

procedure MidiOutMsg(dw: DWord);

{$ifndef Windows}
var MidiEvent: TMidiShortMessage;
{$endif}

  begin
  if (not MidiShallClose) and MidiTest then
    begin
    if (dw and $FF)=0 then
      begin
      if (dw and $FF00)=$100 then
      //// A better text would write the actual tempo
      writeln(TestData,'Tempo: '+IntToHex((dw shr 8) and $FF00,8))
      end
    else
      writeln(TestData,'Data: '+IntToHex(dw,8));
    end
  else if (MidiPositionForward<MidiProgress) or ((dw and $F0)<>$90) then
    begin
{$ifdef Windows}
    MidiOutShortMsg(MidiOutHandle[MidiSelect],dw);
{$else}
    MidiEvent.word:=dw;
    if (dw and 255)=$90 then
    MidiOutShortMsg(MidiEvent.bytes[0],MidiEvent.bytes[1],MidiEvent.bytes[2],MidiEvent.bytes[3])
    else
    if (dw and 255)<>$C8 then              ////
    MidiOutShortMsg(MidiEvent.bytes[0],MidiEvent.bytes[1],MidiEvent.bytes[2],MidiEvent.bytes[3]);
{$endif}
    end;
  end;


end.
