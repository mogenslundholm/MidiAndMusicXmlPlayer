//---------------------------------------------------------------------------
//
//  Program:     MidiAndMusicXmlPlayer.dpr in MidiAndMusicXml.exe
//
//  Project:     MidiAndMusicXmlPlayer.exe and MidiAndMusicXmlPlayer.app
//
//  Purpose:     Main program of MidiAndMusicXmlPlayer
//
//  Compilation: Compile with Delphi 4 (MidiAndMusicXmlPlayer.dpr) or
//               Lazarus (MidiAndMusicXmlPlayer.lpi)
//               or Delphi XE (MidiAndMusicXmlPlayer.dproj)
//
//  Description: If an argument is given then the message window is
//               shown while loading the file. Otherwise a Midi- or
//               MusicXml-file is opened by selecting the file with
//               the open-menu or by dragging the file into the window.
//
//  Files:       MidiAndMusicXmlPlayer.dpr: Start the other windows
//
//               UnitMidiDefinitions.pas: Common definitions
//               UnitMidiGui.pas:  User interface - show the tracks/parts
//                                 select and set panorama
//               UnitMidiIn.pas:   Show the midi input file (decoded)
//               UnitMidiPlay.pas: A thread playin the actual music
//               UnitMidiMixer:    Modify the volume of each track
//               UnitText:         Show the lyrics (if present)
//               UnitXml.pas:      Convert a MusicXml to Midi-events
//               UnitMessage.pas:  A small message window
//               UnitScore.pas:        Shows the notes for test. Only for test.
//                                 (not easy to read)
//               UnitOpenDialog.pas: A not used dialog for opening files
//                                  (shows only Music, Download and Desktop)
//---------------------------------------------------------------------------

program MidiAndMusicXmlPlayer;

uses
  Forms,
{$ifdef FPC}
  Interfaces,
{$endif}
  UnitMidiGui in 'UnitMidiGui.pas' {FormMidiGui},
  UnitMidiIn in 'UnitMidiIn.pas' {FormMidiIn},
  UnitMidiMixer in 'UnitMidiMixer.pas' {FormMidiMixer},
  UnitMidiPlay in 'UnitMidiPlay.pas',
  UnitMidiDefinitions in 'UnitMidiDefinitions.pas',
  UnitText in 'UnitText.pas' {FormText},
  UnitScore in 'UnitScore.pas' {FormScore},
  UnitXml in 'UnitXml.pas',
  UnitMessage in 'UnitMessage.pas' {FormMessage},
  UnitMidiOpenDialog in 'UnitMidiOpenDialog.pas' {FormMidiOpenDialog};

{$R *.RES}

begin
  Application.Initialize;
  if ParamCount>0 then
    begin
    Application.CreateForm(TFormMessage, FormMessage);
    Application.CreateForm(TFormMidiIn, FormMidiIn);
  Application.CreateForm(TFormMidiGui, FormMidiGui);
    end
  else
    begin
    Application.CreateForm(TFormMidiGui, FormMidiGui);
    Application.CreateForm(TFormMidiIn, FormMidiIn);
    Application.CreateForm(TFormMessage, FormMessage);
    end;
  Application.CreateForm(TFormMidiMixer, FormMidiMixer);
  Application.CreateForm(TFormText, FormText);
  Application.CreateForm(TFormScore, FormScore);
  Application.CreateForm(TFormMidiOpenDialog, FormMidiOpenDialog);
  Application.Run;
end.


