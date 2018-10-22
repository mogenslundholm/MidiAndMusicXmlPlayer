program MidiAndMusicXmlPlayerTest;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFNDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  UnitMidiAndMusicXmlPlayerTest in 'UnitMidiAndMusicXmlPlayerTest.pas' {FormMidiAndMusicXmlPlayerTest},
  UnitPlayProcess in 'UnitPlayProcess.pas',
  UnitSemaphores in 'UnitSemaphores.pas',
  UnitFileCentral in 'UnitFileCentral.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormMidiAndMusicXmlPlayerTest, FormMidiAndMusicXmlPlayerTest);
  Application.Run;
end.
