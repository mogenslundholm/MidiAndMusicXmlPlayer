//---------------------------------------------------------------------------
//
//  Program:     UnitMidiIn.pas in MidiAndMusicXmlPlayer
//
//  Project:     MidiAndMusicXmlPlayer.exe and MidiAndMusicXmlPlayer.app
//
//  Purpose:     To provide a window to show the decoded midi file
//
//  Compilation: Compile with Delphi 4 (MidiAndMusicXmlPlayer.dpr) or
//               Lazarus (MidiAndMusicXmlPlayer.lpi)
//               or Delphi XE (MidiAndMusicXmlPlayer.dproj)
//
//  Description: The window memo field is filled with data by UnitMidiGui
//
//---------------------------------------------------------------------------

unit UnitMidiIn;
{$ifdef FPC}
{$MODE Delphi}
{$endif}

interface

uses
{$ifndef FPC}
  Windows, 
{$else}
  LCLIntf, LCLType, LMessages, 
{$endif}  
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormMidiIn = class(TForm)
    MemoMidiIn: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMidiIn: TFormMidiIn;

implementation

{$ifdef FPC}
{$R *.lfm}

{$else}
{$R *.DFM}
{$endif}
// Used to show the decoded midi file

end.
